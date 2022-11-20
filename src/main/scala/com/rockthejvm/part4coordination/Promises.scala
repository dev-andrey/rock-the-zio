package com.rockthejvm.part4coordination

import zio.*
import com.rockthejvm.utils.*

object Promises extends ZIOAppDefault {
  val aPromise = Promise.make[Throwable, Long]

  // await - block a fiber until the promise has a value
  val reader = aPromise.flatMap { promise =>
    promise.await
  }

  // succeed, fail, complete
  val writer = aPromise.flatMap { promise =>
    promise.succeed(42L)
  // promise.fail(new RuntimeException())
  // promise.complete(???)
  }

  def demoPromise = {
    // producer/consumer problem
    def consumer(promise: Promise[Throwable, Long]) =
      for {
        _   <- ZIO.succeed("[consumer] waiting for result...").debugThread
        mol <- promise.await
        _   <- ZIO.succeed(s"[consumer] I got the result $mol").debugThread
      } yield ()

    def producer(promise: Promise[Throwable, Long]) =
      for {
        _   <- ZIO.succeed("[producer] crunching numbers...").debugThread
        _   <- ZIO.sleep(3.seconds)
        _   <- ZIO.succeed("[producer] complete").debugThread
        mol <- ZIO.succeed(42L)
        _   <- promise.succeed(mol)
      } yield ()

    for {
      promise <- Promise.make[Throwable, Long]
      _       <- consumer(promise) <&> producer(promise)
    } yield ()
  }

  /*
    - purely functional block on a fiber until a signal from another fiber
    - waiting on a value which may not yet be available, without thread starvation
    - inter-fiber communication
   */

  // simulate downloading from multiple parts
  val fileParts = List("I ", "love S", "cala", " with pure FP an", "d ZIO! <EOF>")

  // this creates busy work that we have to orchestrate ourselves
  def downloadFileWithRef = {
    def downloadFile(contentRef: Ref[String]): UIO[Unit] =
      ZIO.collectAllDiscard(
        fileParts.map { part =>
          ZIO.succeed(s"got $part").debugThread *> ZIO.sleep(1.second) *> contentRef.update(_ + part)
        }
      )

    def notifyFileComplete(contentRef: Ref[String]): UIO[Unit] =
      for {
        file <- contentRef.get
        _    <- if (file.endsWith("<EOF>")) ZIO.succeed("Completed").debugThread
                else ZIO.succeed("downloading...").debugThread *> ZIO.sleep(500.millis) *> notifyFileComplete(contentRef)
      } yield ()

    for {
      contentRef <- Ref.make("")
      _          <- downloadFile(contentRef) zipPar notifyFileComplete(contentRef)
    } yield ()
  }

  def downloadFileWithRefAndPromise = {
    def downloadFile(contentRef: Ref[String], promise: Promise[Throwable, String]) =
      ZIO.collectAllDiscard(
        fileParts.map { part =>
          for {
            _    <- ZIO.succeed(s"got $part").debugThread
            _    <- ZIO.sleep(1.second)
            file <- contentRef.updateAndGet(_ + part)
            _    <- promise.succeed(file).when(file.endsWith("<EOF>"))
          } yield ()
        }
      )

    def notifyFileComplete(promise: Promise[Throwable, String]) =
      for {
        _    <- ZIO.succeed("downloading...").debugThread
        file <- promise.await
        _    <- ZIO.succeed(s"file download complete: $file").debugThread
      } yield ()

    for {
      contentRef <- Ref.make("")
      promise    <- Promise.make[Throwable, String]
      _          <- downloadFile(contentRef, promise) <&> notifyFileComplete(promise)
    } yield ()
  }

  /** Exercises
    *
    *   1. write a simulated "egg timer" with two ZIOs
    *      - one increments a counter every 1s
    *      - one waits for the counter to become 10, after which it will "ring a bell"
    */
  def eggTimer: UIO[Unit] = {
    def ticking(ref: Ref[Int], promise: Promise[Throwable, Int]): UIO[Unit] =
      for {
        _    <- ZIO.sleep(100.millis)
        tick <- ref.updateAndGet(_ + 1).debugThread
        _    <- if (tick == 10) promise.succeed(10)
                else ticking(ref, promise)
      } yield ()

    def ring(promise: Promise[Throwable, Int]) =
      for {
        _ <- promise.await
        _ <- Console.printLine("It happened")
      } yield ()

    for {
      ref     <- Ref.make(0)
      promise <- Promise.make[Throwable, Int]
      fiber   <- ticking(ref, promise).fork
      _       <- ring(promise)
      _       <- fiber.interrupt
    } yield ()
  }.orDie

  /** 2. Write a "race pair"
    *   - use a Promise which can hold an Either[exit for A, exit for B]
    *   - start a fiber for each ZIO
    *   - on completion (with any status), each ZIO needs to complete that promise (hint: use a finalizer)
    *   - waiting on the Promise's value can be interrupted!
    *   - if the whole race is interrupted, interrupt the running fibers
    */
  def racePair[R, E, A, B](
      zioA: => ZIO[R, E, A],
      zioB: => ZIO[R, E, B]
  ): URIO[R, Either[(Exit[E, A], Fiber[E, B]), (Fiber[E, A], Exit[E, B])]] =
    ZIO.uninterruptibleMask { restore =>
      for {
        promise <- Promise.make[Nothing, Either[Exit[E, A], Exit[E, B]]]
        fibA    <- zioA.onExit(exitA => promise.succeed(Left(exitA))).fork
        fibB    <- zioB.onExit(exitB => promise.succeed(Right(exitB))).fork
        result  <- restore(promise.await)
                     .onInterrupt {
                       for {
                         interruptA <- fibA.interrupt.fork
                         interruptB <- fibB.interrupt.fork
                         _          <- interruptA.join
                         _          <- interruptB.join
                       } yield ()
                     }
      } yield result match
        case Left(exitA)  => Left((exitA, fibB))
        case Right(exitB) => Right((fibA, exitB))
    }

  def run =
    racePair(ZIO.succeed(42).delay(500.millis), ZIO.succeed(420).delay(1.second)).debugThread
}
