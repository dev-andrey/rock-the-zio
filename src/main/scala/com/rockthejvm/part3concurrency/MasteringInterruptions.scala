package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*

object MasteringInterruptions extends ZIOAppDefault:

  // Interruptions are important
  // fib.interrupt
  // ZIO.race, ZIO.zipPar, ZIO.collectAllPar
  // outliving parent fiber

  // manual interruptions
  val aManuallyInterruptedZIO = ZIO.succeed("computing...").debugThread *> ZIO.interrupt *> ZIO.succeed(42L).debugThread

  // finalizer
  val effWithInterruptionFinalizer = aManuallyInterruptedZIO.onInterrupt(ZIO.succeed("I was interrupted!").debugThread)

  // uninterruptible
  // payment flow to NOT be interrupted
  val fussyPaymentSystem =
    (for
      _ <- ZIO.succeed("payment running, don't cancel me...").debugThread
      _ <- ZIO.sleep(1.second)
      _ <- ZIO.succeed("payment completed").debugThread
    yield ()).onInterrupt {
      ZIO.succeed("MEGA CANCEL OF DOOM!").debugThread // don't want to trigger it
    }

  val cancellationOfDoom =
    for
      fib <- fussyPaymentSystem.fork
      _   <- fib.interrupt.delay(500.millis)
      _   <- fib.join
    yield ()

  val atomicPayment    = ZIO.uninterruptible(fussyPaymentSystem) // make a ZIO atomic
  val atomicPayment_v2 = fussyPaymentSystem.uninterruptible      // make a ZIO atomic (alt)

  val noCancelProcess =
    for
      fib <- atomicPayment.fork
      _   <- fib.interrupt.delay(500.millis)
      _   <- fib.join
    yield ()

  // interruptibility is regional
  val zio1         = ZIO.succeed(1)
  val zio2         = ZIO.succeed(2)
  val zio3         = ZIO.succeed(3)
  val zioComposed  = (zio1 *> zio2 *> zio3).uninterruptible               // all the effs are uninterruptible
  val zioComposed2 = (zio1 *> zio2.interruptible *> zio3).uninterruptible // inner scopes override outer scopes

  // uninterruptibleMask - powerful way to define what can be interrupted
  /* example: an authentication service
      - input password, can be interrupted, because otherwise it might block the fiber indefinitely
      - verify password, which cannot be interrupted once it's triggered
   */
  val inputPassword =
    for
      _    <- ZIO.succeed("input password:").debugThread
      _    <- ZIO.succeed("(typing password)").debugThread
      _    <- ZIO.sleep(2.seconds)
      pass <- ZIO.succeed("RockTheJVM1!")
    yield pass

  def verifyPassword(password: String) =
    for
      _      <- ZIO.succeed("verifying...").debugThread
      _      <- ZIO.sleep(2.seconds)
      result <- ZIO.succeed(password == "RockTheJVM1!")
    yield result

  val authFlow = ZIO.uninterruptibleMask { restore =>
    // EVERYTHING is uninterruptible, except for stuff wrapped in `restore(...)`
    // restores interruptibility at the time of the call
    for
      pass         <- restore(inputPassword).onInterrupt(ZIO.succeed("Authentication timed out. Try again later").debugThread)
      verification <- verifyPassword(pass)
      _            <- if (verification) ZIO.succeed("Auth successful.").debugThread
                      else ZIO.succeed("Auth failed.").debugThread
    yield ()
  }

  val authProgram =
    for
      authFib <- authFlow.fork
      _       <- ZIO.succeed("Attempting to cancel auth...").debugThread.delay(3.seconds) *> authFib.interrupt
      _       <- authFib.join
    yield ()

  /** Exercise
    */
  // 1
  val cancelBeforeMol =
    ZIO.interrupt *> ZIO.succeed(42).debugThread // do nothing
  val unCancelBeforeMol =
    (ZIO.interrupt *> ZIO.succeed(42).debugThread).uninterruptible // do nothing, as .interrupt is inside

  // 2
  // restore(inputPassword) is interruptible gap, but outer uninterruptibleMask fills the gap
  val authProgram_v2 =
    for
      authFib <- ZIO.uninterruptibleMask(_ => authFlow).fork
      _       <- ZIO.succeed("attempting to cancel...").delay(3.seconds) *> authFib.interrupt
      _       <- authFib.join
    yield ()

  // 3
  val threeStepProgram = {
    val sequence = ZIO.uninterruptibleMask { restore =>
      for
        _ <- restore(ZIO.succeed("interruptible").debugThread *> ZIO.sleep(1.second))
        _ <- ZIO.succeed("uninterruptible").debugThread *> ZIO.sleep(1.second)
        _ <- restore(ZIO.succeed("interruptible 2").debugThread *> ZIO.sleep(1.second))
      yield ()
    }
    for
      fib <- sequence.fork
      _   <- ZIO.succeed("interrupting...").delay(1500.millis) *> fib.interrupt
      _   <- fib.join
    yield ()
  }

  def run = threeStepProgram
