package com.rockthejvm.part4coordination

import zio.*
import com.rockthejvm.utils.*

object Semaphores extends ZIOAppDefault {

  // n permits
  // acquire, acquireN
  // release, releaseN
  val aSemaphore = Semaphore.make(10)

  // limit number of concurrent sessions on any resource
  def doWorkWhileLoggedIn(): UIO[Int] =
    Random.nextIntBounded(100).delay(1.second)

  def login(id: Int, sem: Semaphore) =
    ZIO.succeed(s"[task $id] waiting to log in").debugThread *>
      sem.withPermit { // acquire + zio + release
        for {
          // critical section start
          _   <- ZIO.succeed(s"[task $id] logged in, working...").debugThread
          res <- doWorkWhileLoggedIn()
          _   <- ZIO.succeed(s"[task $id] done: $res ").debugThread
        } yield res
      }

  def loginWeighted(n: Int, sem: Semaphore) =
    ZIO.succeed(s"[task $n] waiting to log in with $n permits").debugThread *>
      sem.withPermits(n) { // acquire + zio + release
        for {
          // critical section start when you acquired ALL n permits
          _   <- ZIO.succeed(s"[task $n] logged in, working...").debugThread
          res <- doWorkWhileLoggedIn()
          _   <- ZIO.succeed(s"[task $n] done: $res ").debugThread
        } yield res
      }

  def demoSemaphore =
    for {
      sem <- Semaphore.make(2) // Semaphore.make(1) == a Mutex
      f1  <- login(1, sem).fork
      f2  <- login(2, sem).fork
      f3  <- login(3, sem).fork
      _   <- f1.join
      _   <- f2.join
      _   <- f3.join
    } yield ()

  def demoSemaphoreWeighted =
    for {
      sem <- Semaphore.make(2) // Semaphore.make(1) == a Mutex
      f1  <- loginWeighted(1, sem).fork
      f2  <- loginWeighted(2, sem).fork
      f3  <- loginWeighted(3, sem).fork
      _   <- f1.join
      _   <- f2.join
      _   <- f3.join
    } yield ()

  /** Exercise
    * -what is the code SUPPOSED to do?
    *
    *   - find if there is anyhting wrong
    *
    * -fix the problem
    */
  val mySemaphore = Semaphore.make(1)
  val tasks       = mySemaphore.flatMap { sem =>
    ZIO.foreachPar(1 to 10) { id =>
      for {
        _      <- ZIO.succeed(s"[task $id] waiting to log in").debugThread
        result <- sem.withPermit {
                    for {
                      // critical section start
                      _   <- ZIO.succeed(s"[task $id] logged in, working...").debugThread
                      res <- doWorkWhileLoggedIn()
                      _   <- ZIO.succeed(s"[task $id] done: $res ").debugThread
                    } yield res
                  }
      } yield result
    }.debugThread
  }

  def run = tasks
}
