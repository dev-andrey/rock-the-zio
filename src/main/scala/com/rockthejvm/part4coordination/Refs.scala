package com.rockthejvm.part4coordination

import zio.*
import com.rockthejvm.utils.*
import jdk.vm.ci.hotspot.JFR.Ticks

import java.util.concurrent.TimeUnit

object Refs extends ZIOAppDefault {

  val atomicMOL = Ref.make(42L)

  // obtain a value
  val mol = atomicMOL.flatMap { ref =>
    ref.get // returns UIO[Long], thread-safe getter
  }

  // changing
  val setMol = atomicMOL.flatMap { ref =>
    ref.set(100L) // UIO[Long], thread-safe setter
  }

  // get + change in ONE atomic operation
  val getAndSetMol = atomicMOL.flatMap { ref =>
    ref.getAndSet(500) // returns OLD value
  }

  // update - run a function on the value
  val updateMol = atomicMOL.flatMap { ref =>
    ref.update(_ * 100)
  }

  // update and get
  val updateAndGet = atomicMOL.flatMap { ref =>
    ref.updateAndGet(_ * 100) // returns the NEW value
    ref.getAndUpdate(_ * 100) // returns teh OLD value
  }

  def demoConcurrentWorkImpure(): UIO[Unit] = {
    var count = 0

    def task(workload: String): UIO[Unit] = {
      val wordCount = workload.split(" ").length

      for {
        _        <- ZIO.succeed(s"Counting words for: $workload: $wordCount").debugThread
        newCount <- ZIO.succeed(count + wordCount)
        _        <- ZIO.succeed(s"new total: $newCount").debugThread
        _        <- ZIO.succeed(count += wordCount)
      } yield ()
    }

    val effects = List("awesome awesome", "zio is very cool", "coding is awesome").map(task)

    ZIO.collectAllParDiscard(effects)
  }

  def demoConcurrentWorkPure(): UIO[Unit] = {

    def task(workload: String, total: Ref[Int]): UIO[Unit] = {
      val wordCount = workload.split(" ").length

      for {
        _        <- ZIO.succeed(s"Counting words for: $workload: $wordCount").debugThread
        newCount <- total.updateAndGet(_ + wordCount)
        _        <- ZIO.succeed(s"new total: $newCount").debugThread
      } yield ()
    }

    for {
      initialCount <- Ref.make(0)
      _            <- ZIO.collectAllParDiscard(
                        List("awesome awesome", "zio is very cool", "coding is awesome")
                          .map(str => task(str, initialCount))
                      )
    } yield ()
  }

  /** Exercises
    */
  // 1
  def tickingClock = {

    // print the current time every 1s + increase a counter ("ticks")
    // print the total ticks count every 5s

    def tickingClock(ticks: Ref[Long]): UIO[Unit] =
      for {
        _ <- ZIO.sleep(1.second)
        _ <- Clock.currentTime(TimeUnit.MILLISECONDS).debugThread
        _ <- ticks.updateAndGet(_ + 1)
        _ <- tickingClock(ticks)
      } yield ()

    def printTicks(ticks: Ref[Long]): UIO[Unit] =
      for {
        _ <- ZIO.sleep(5.seconds)
        _ <- ZIO.succeed(s"TICKS: $ticks").debugThread
        _ <- printTicks(ticks)
      } yield ()

    for {
      tickCount <- Ref.make(0L)
      _         <- (tickingClock(tickCount) zipPar printTicks(tickCount)).unit
    } yield ()

  }

  def run = tickingClock
}
