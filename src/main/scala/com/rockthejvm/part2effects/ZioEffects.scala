package com.rockthejvm.part2effects
import zio.*

import scala.io.StdIn

object ZioEffects {
  // success
  val meaningOfLife: ZIO[Any, Nothing, Int]   = ZIO.succeed(42)
  // failure
  val aFailure: ZIO[Any, String, Nothing]     = ZIO.fail("Something went wrong")
  // suspension/delay
  val aSuspendedZIO: ZIO[Any, Throwable, Int] = ZIO.suspend(meaningOfLife)

  val improvedMOL = meaningOfLife.map(_ * 2)
  val printingMOL = meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))

  // for-comprehension
  val smallProgram =
    for
      _    <- ZIO.succeed(println("what's your name"))
      name <- ZIO.succeed(StdIn.readLine())
      _    <- ZIO.succeed(println(s"Welcome to ZIO, $name"))
    yield ()

  // A LOT of combinators
  // zip, zipWith
  val anotherMOL  = ZIO.succeed(100)
  val tupledZIO   = meaningOfLife.zip(anotherMOL)
  val combinedZIO = meaningOfLife.zipWith(anotherMOL)(_ * _)

  /** Exercises
    */

  // 1 - sequence of two ZIOs and take the value of the last one
  def sequenceTakeLast[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    zioa *> ziob

  def sequenceTakeFirst[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    zioa <* ziob

  // 3 run ZIO forever
  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio *> runForever(zio)

  // 4 convert the value of a ZIO to something else
  def convert[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] =
    zio.as(value)

  // 5 discard zio to Unit
  def asUnit[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] =
    zio.unit

  // sum with stack overflow
  def sum(n: Int): Int =
    if (n == 0) 0
    else n + sum(n - 1) // will crush at sum(2000)

  // 6 sum with zio
  def sumZIO(n: Int): UIO[Int] =
    if (n == 0) ZIO.succeed(0)
    else
      for
        current <- ZIO.succeed(n)
        prevSum <- sumZIO(n - 1)
      yield current + prevSum

  // 7 fibonacci
  def fibZIO(n: Int, isFirst: Boolean): UIO[BigInt] =
    ZIO.debug(s"${if (isFirst) "1: " + n else "2: " + n}") *> {
      if (n <= 2) ZIO.succeed(1)
      else
        for
          minusOne <- fibZIO(n - 1, isFirst)
          _        <- ZIO.debug("after fibZIO(n-1)")
          minusTwo <- fibZIO(n - 2, !isFirst)
          _        <- ZIO.debug("after fibZIO(n-2)")
        yield minusOne + minusTwo
    }

  def main(args: Array[String]): Unit = {
    val runtime        = zio.Runtime.default
    given trace: Trace = Trace.empty
    Unsafe.unsafe { unsafe =>
      given u: Unsafe = unsafe

      val firstEffect  = ZIO.succeed {
        println("computing first effect")
        Thread.sleep(1000)
        1
      }
      val secondEffect = ZIO.succeed {
        println("computing second effect")
        Thread.sleep(1000)
        2
      }

      println(
        runtime.unsafe
          .run {
            // sequenceTakeLast(firstEffect, secondEffect)
            fibZIO(5, true)
          }
      )
    }
  }

}
