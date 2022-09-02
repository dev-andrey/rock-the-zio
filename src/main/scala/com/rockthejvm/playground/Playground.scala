package com.rockthejvm.playground
import zio.*

import java.io.IOException

object Playground extends ZIOAppDefault:
  def decrement(n: Int): UIO[Int] =
    if (n == 0) ZIO.debug("Done").as(0)
    else ZIO.debug(s"$n") *> decrement(n - 1)

  val notSuspendedEffect = {
    scala.Console.println("Not suspended Hello World!")
    decrement(100)
  }

  val suspendedEffect =
    ZIO.suspend {
      scala.Console.println("Suspended Hello World!")
      decrement(100)
    }

  val suspend2 = decrement(100)

  val run = {
    val eff = notSuspendedEffect
    for {
      _      <- ZIO.debug("getting random number")
      random <- Random.nextBoolean
      _      <- ZIO.debug(s"random was $random")
      _      <- if (random) eff else Console.printLine(":(")
    } yield ()
  }
