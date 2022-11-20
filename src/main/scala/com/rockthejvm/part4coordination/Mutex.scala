package com.rockthejvm.part4coordination

import zio.*
import com.rockthejvm.utils.*

import scala.collection.immutable

abstract class Mutex {
  def acquire: UIO[Unit]
  def release: UIO[Unit]
}
object Mutex         {
  type Signal = Promise[Nothing, Unit]
  final case class State(locked: Boolean, waiting: immutable.Queue[Signal])

  val unlocked = State(locked = false, immutable.Queue())

  def make: UIO[Mutex] = Ref.make(unlocked).map(createInterruptableMutex)

  def createInterruptableMutex(state: Ref[State]) = new Mutex {

    override def acquire = ZIO.uninterruptibleMask { restore =>
      Promise.make[Nothing, Unit].flatMap { signal =>

        val cleanup: UIO[Unit] =
          state.modify { case State(flag, waiting) =>
            val newWaiting = waiting.filterNot(_ eq signal)

            // blocked only if newWaiting != waiting => release the mutex
            val wasBlocked = newWaiting != waiting
            val decision   = if (wasBlocked) ZIO.unit else release

            decision -> State(flag, newWaiting)
          }.flatten

        state.modify {
          case State(false, _)      =>
            ZIO.unit -> State(true, immutable.Queue[Signal]())
          case State(true, waiting) =>
            restore(signal.await).onInterrupt(cleanup) -> State(true, waiting.enqueue(signal))
        }.flatten
      }
    }

    override def release =
      state.modify {
        case State(false, _)                         => ZIO.unit -> unlocked
        case State(true, waiting) if waiting.isEmpty => ZIO.unit -> unlocked
        case State(true, waiting)                    =>
          val (head, remaining) = waiting.dequeue
          head.succeed(()).unit -> State(true, remaining)
      }.flatten
  }

  def createSimpleMutex(state: Ref[State]): Unit =
    new Mutex {
      override def acquire =
        Promise.make[Nothing, Unit].flatMap { signal =>
          state.modify {
            case State(false, _)      => ZIO.unit     -> State(true, immutable.Queue[Signal]())
            case State(true, waiting) => signal.await -> State(true, waiting.enqueue(signal))
          }.flatten
        }

      override def release =
        state.modify {
          case State(false, _)                         => ZIO.unit -> unlocked
          case State(true, waiting) if waiting.isEmpty => ZIO.unit -> unlocked
          case State(true, waiting)                    =>
            val (head, remaining) = waiting.dequeue
            head.succeed(()).unit -> State(true, remaining)
        }.flatten
    }
}

object MutexPlayground extends ZIOAppDefault {

  def workInCriticalRegion: UIO[Int] =
    Random.nextIntBounded(100).delay(1.second)

  def demoNonLockingTasks =
    ZIO.collectAllParDiscard((1 to 10).toList.map { i =>
      for {
        _      <- ZIO.succeed(s"[task $i] working...").debugThread
        result <- workInCriticalRegion
        _      <- ZIO.succeed(s"[task $i] got results: $result").debugThread
      } yield ()
    })

  def createTask(id: Int, mutex: Mutex): UIO[Int] = {
    val task = for {
      _      <- ZIO.succeed(s"[task $id] waiting for mutex...").debugThread
      _      <- mutex.acquire
      // critical region
      _      <- ZIO.succeed(s"[task $id] working...").debugThread
      result <- workInCriticalRegion.onInterrupt(mutex.release)
      _      <- ZIO.succeed(s"[task $id] got results: $result").debugThread
      // end of critical region
      _      <- mutex.release
    } yield result

    task
      .onInterrupt(ZIO.succeed(s"[task $id] was interrupted").debugThread)
      .onError(cause => ZIO.succeed(s"[task $id] ended in error: $cause"))
  }

  def demoLockingTasks =
    for {
      mutex <- Mutex.make
      _     <- ZIO.collectAllParDiscard((1 to 10).toList.map(i => createTask(i, mutex)))
    } yield ()

  def createInterruptingTask(id: Int, mutex: Mutex): UIO[Int] =
    if (id % 2 == 0)
      createTask(id, mutex)
    else
      for {
        fib    <- createTask(id, mutex).fork
        _      <- ZIO.succeed(s"interrupting $id").debugThread.delay(2500.millis) *> fib.interrupt
        result <- fib.join
      } yield result

  def demoInterruptingTasks =
    for {
      mutex <- Mutex.make
      fib1  <- createInterruptingTask(1, mutex).fork
      fib2  <- createInterruptingTask(2, mutex).fork
      fib3  <- createInterruptingTask(3, mutex).fork
      fib4  <- createInterruptingTask(4, mutex).fork
      fib5  <- createInterruptingTask(5, mutex).fork
      fib6  <- createInterruptingTask(6, mutex).fork
      fib7  <- createInterruptingTask(7, mutex).fork
      fib8  <- createInterruptingTask(8, mutex).fork
      fib9  <- createInterruptingTask(9, mutex).fork
      fib10 <- createInterruptingTask(10, mutex).fork
      _     <- fib1.await
      _     <- fib2.await
      _     <- fib3.await
      _     <- fib4.await
      _     <- fib5.await
      _     <- fib6.await
      _     <- fib7.await
      _     <- fib8.await
      _     <- fib9.await
      _     <- fib10.await
    } yield ()

  def run = demoInterruptingTasks
}
