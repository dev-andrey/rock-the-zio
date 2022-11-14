package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*

object Schedules extends ZIOAppDefault:

  val aZIO = Random.nextBoolean.flatMap { flag =>
    if (flag) ZIO.succeed("fetched value!").debugThread
    else ZIO.succeed("failure...").debugThread *> ZIO.fail("error")
  }

  val aRetriedZIO = aZIO.retry(Schedule.recurs(10)) // tries 10 times

  val oneTimeSchedule    = Schedule.once
  val recurrentSchedule  = Schedule.recurs(10)
  val fixedInterval      = Schedule.spaced(1.second)    // repeats every second until a success
  val exponentialBackoff = Schedule.exponential(1.second, 2.0)
  val fiboSchedule       = Schedule.fibonacci(1.second) // 1s, 1s, 2s, 3s, 5s, ...

  // combinators
  val recurrentAndSpaced = Schedule.recurs(3) && Schedule.spaced(1.second)

  // sequence
  val recurrentThenSpaced = Schedule.recurs(3) ++ Schedule.spaced(1.second)

  val totalElapsed = Schedule.spaced(1.second) >>> Schedule.elapsed.map(time => println(s"total time elapsed: $time"))

  def run = aZIO.retry(totalElapsed)
