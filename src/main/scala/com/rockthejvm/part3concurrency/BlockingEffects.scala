package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*

import java.util.concurrent.atomic.AtomicBoolean

object BlockingEffects extends ZIOAppDefault:

  def blockingTask(n: Int): UIO[Unit] =
    ZIO.succeed(s"running blocking task $n").debugThread *>
      ZIO.succeed(Thread.sleep(10_000)) *>
      blockingTask(n)

  // thread starvation
  val program = ZIO.foreachPar((1 to 100).toList)(blockingTask)

  // blocking thread pool
  val aBlockingZIO = ZIO.attemptBlocking {
    println(s"[${Thread.currentThread().getName}] running a long computation...")
    Thread.sleep(10_000)
    println(s"[${Thread.currentThread().getName}] computation finished!!!")
    42
  }

  // blocking code usually cannot be interrupted
  val tryInterrupting =
    for
      blockingFib <- aBlockingZIO.fork
      _           <- ZIO.succeed("interrupting...").debugThread.delay(1.second) *> blockingFib.interrupt
      mol         <- blockingFib.join
    yield mol

  // can use attemptBlockingInterrupt
  // based on Thread.interrupt -> InterruptedException
  val aBlockingInterruptableZIO =
    ZIO.attemptBlockingInterrupt {
      println(s"[${Thread.currentThread().getName}] running a long computation...")
      Thread.sleep(10_000)
      println(s"[${Thread.currentThread().getName}] computation finished!!!")
      42
    }

  val tryInterrupting_v2 =
    for
      blockingFib <- aBlockingInterruptableZIO.fork
      _           <- ZIO.succeed("interrupting...").debugThread.delay(1.second) *> blockingFib.interrupt
      mol         <- blockingFib.join
    yield mol

  // set a flag/switch
  def interruptibleBlockingEffect(cancelledFlag: AtomicBoolean): Task[Unit] =
    ZIO.attemptBlockingCancelable {
      (1 to 100_000).foreach { elem =>
        if (!cancelledFlag.get())
          println(elem)
          Thread.sleep(100)
      }
    }(ZIO.succeed(cancelledFlag.set(true))) // cancelling/interrupting effect

  val interruptibleBlockingDemo =
    for
      fib <- interruptibleBlockingEffect(new AtomicBoolean(false)).fork
      _   <- ZIO.sleep(2.seconds) *> ZIO.succeed("interrupting...").debugThread *> fib.interrupt
      _   <- fib.join
    yield ()

  // SEMANTIC blocking - no blocking of threads. de-schedule the effect/fiber

  val sleeping       = ZIO.sleep(1.second)             // SEMANTICALLY blocking, interruptible
  val sleepingThread = ZIO.succeed(Thread.sleep(1000)) // blocking, uninterruptible

  // yield (same thread)
  val chainedZIO = (1 to 1000).map(int => ZIO.succeed(int)).reduce(_.debugThread *> _.debugThread)

  // zio tries to not re-schedule to another thread
  val yieldingDemo = (1 to 100).map(int => ZIO.succeed(int)).reduce(_.debugThread *> ZIO.yieldNow *> _.debugThread)

  def run = yieldingDemo
