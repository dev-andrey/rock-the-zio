package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*

object Interruptions extends ZIOAppDefault:

  val zioWithTime =
    (
      ZIO.succeed("starting computation").debugThread *>
        ZIO.sleep(2.seconds) *>
        ZIO.succeed(42).debugThread
    )
      .onInterrupt(
        ZIO.succeed("I was interrupted!").debugThread
      )
//      .onDone(
//        err => ZIO.succeed("Done with errors").debugThread,
//        v => ZIO.succeed("I am done!").debugThread
//      )

  val interruption =
    for
      fib <- zioWithTime.fork
      _   <- ZIO.sleep(1.second)
      _   <- ZIO.succeed("Interrupting").debugThread
      // _   <- fib.interrupt // <- interrupt is an effect (blocks the calling fiber)
      _   <- fib.interruptFork // <- interrupt without blocking
      _   <- ZIO.succeed("Interruption Successful").debugThread
      res <- fib.join
    yield res

  /*
    Automatic interruption
   */
  // outliving a parent fiber
  val parentEffect =
    ZIO.succeed("spawning fiber").debugThread *>
      // zioWithTime.fork *> // child fiber
      zioWithTime.forkDaemon *> // child of the main application
      ZIO.sleep(1.second) *>
      ZIO.succeed("parent successful").debugThread

  val testOutlivingParent =
    for
      fib <- parentEffect.fork
      _   <- ZIO.sleep(3.seconds)
      _   <- fib.join
    yield ()
  // RESULT: child fibers will be interrupted (automatically) if parent fiber is completed

  /*
    racing
      start multiple, pick winner and interrupt others
   */
  val slowEff =
    ZIO
      .succeed("slow")
      .debugThread
      .delay(2.seconds)
      .onInterrupt(ZIO.succeed("[slow] interrupted").debugThread)

  val fastEff =
    ZIO
      .succeed("fast")
      .debugThread
      .delay(1.seconds)
      .onInterrupt(ZIO.succeed("[fast] interrupted").debugThread)

  val aRace = slowEff race fastEff

  val testRace = aRace.fork *> ZIO.sleep(3.seconds)

  /** Exercise
    */
  // 1 - implement a timeout function
  //  - if zio is successful before timeout => a successful effect
  //  - if zio fails before timeout => a failed effect
  //  - if zio takes longer than timeout => interrupt
  def timeout[R, E, A](zio: ZIO[R, E, A], time: Duration): ZIO[R, E, A] =
    for
      fib <- zio.fork
      _   <- fib.interruptFork.delay(time)
      res <- fib.join
    yield res

  // 2 - timeout v2
  //  - if zio is successful before timeout => a successful effect
  //  - if zio fails before timeout => a failed effect
  //  - if zio takes longer than timeout => a success with None
  def timeout_v2[R, E, A](zio: ZIO[R, E, A], time: Duration): ZIO[R, E, Option[A]] =
    zio.map(Some(_)) race ZIO.none.delay(time)

  def run = timeout_v2(ZIO.succeed(42).delay(2.second), 1.seconds).debugThread
