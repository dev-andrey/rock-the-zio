package com.rockthejvm.part3concurrency
import zio.*
import com.rockthejvm.utils.*

import java.io.{File, FileReader, FileWriter}
import scala.io.Source

object Fibers extends ZIOAppDefault:
  val meaningOfLife = ZIO.succeed(42)
  val favLang       = ZIO.succeed("Scala")

  // Fiber = lightweight thread
  def createFiber: Fiber[Throwable, String] = ??? // almost impossible to create manually

  val sameThreadIO =
    for
      mol  <- meaningOfLife.debugThread
      lang <- favLang.debugThread
    yield (mol, lang)

  val diffThreadIO =
    for
      _ <- meaningOfLife.debugThread.fork
      _ <- favLang.debugThread.fork
      _ <- ZIO.sleep(500.millis).debugThread
    yield ()

  val meaningOfLifeFiber: ZIO[Any, Nothing, Fiber[Throwable, Int]] =
    meaningOfLife.fork

  // join fiber
  def runOnAnotherThread[R, E, A](zio: ZIO[R, E, A]) =
    for
      fib <- zio.fork
      res <- fib.join
    yield res

  // await a fiber
  def runOnAnotherThread_v2[R, E, A](zio: ZIO[R, E, A]) =
    for
      fib <- zio.fork
      res <- fib.await
    yield res match
      case Exit.Success(value) => s"succeed with $value"
      case Exit.Failure(cause) => s"failed with $cause"

  // poll - peek at the result of the fiber RIGHT NOW, without blocking
  val peekFiber =
    for
      fib     <- ZIO.attempt {
                   Thread.sleep(1000)
                   42
                 }.fork
      result  <- fib.poll
      _       <- ZIO.sleep(1.second)
      result2 <- fib.poll
    yield (result, result2)

  // compose fibers
  val zippedFibers =
    for
      fib1  <- ZIO.succeed("Result from fiber 1").debugThread.fork
      fib2  <- ZIO.succeed("Result from fiber 2").debugThread.fork
      fiber  = fib1 zip fib2
      tuple <- fiber.join
    yield tuple

  // orElse
  val chainedFibers =
    for
      fib1 <- ZIO.fail("not good!").debugThread.fork
      fib2 <- ZIO.succeed("Rock the JVM!").debugThread.fork
      fiber = fib1 orElse fib2
      res  <- fiber.join
    yield res

  /** Exercises
    */
  // 1 - zip two fibers without using zip
  // hint: create a fiber that wait for both fibers
  def zipFibers[E, A, B](fib1: Fiber[E, A], fib2: Fiber[E, B]): ZIO[Any, Nothing, Fiber[E, (A, B)]] =
    (
      for
        v1 <- fib1.join
        v2 <- fib2.join
      yield (v1, v2)
    ).fork

  def zipFibersGeneric[E, E1 <: E, E2 <: E, A, B](
      fib1: Fiber[E1, A],
      fib2: Fiber[E2, B]
  ): ZIO[Any, Nothing, Fiber[E, (A, B)]] =
    (for
      v1 <- fib1.join
      v2 <- fib2.join
    yield (v1, v2)).fork

  // 2 - same thing with orElse
  def chainFibers[E, A](fib1: Fiber[E, A], fib2: Fiber[E, A]): ZIO[Any, Nothing, Fiber[E, A]] =
    fib1.join.orElse(fib2.join).fork

  // 3 - distributing a task in between many fibers
  // spawn n fibers, count the n of words in each file
  // then aggregate all the results together in one big number
  def generateRandomFile(path: String): Unit =
    val random = scala.util.Random
    val chars  = 'a' to 'z'
    val nWords = random.nextInt(2000) // at most 2000 random words

    val content = (1 to nWords)
      .map { _ =>
        (1 to random.nextInt(10))
          .map(_ => chars(random.nextInt(26)))
          .mkString
      }
      .mkString(" ")

    val writer = new FileWriter(new File(path))
    writer.write(content)
    writer.flush()
    writer.close()

  val generator =
    ZIO.foreachParDiscard(1 to 10) { i =>
      ZIO.attempt(generateRandomFile(s"src/main/resources/test$i.txt"))
    }

  def countWords(path: String) =
    ZIO.fromAutoCloseable(ZIO.attempt(Source.fromFile(new File(path)))).map { source =>
      source.getLines().mkString(" ").split(' ').count(_.nonEmpty)
    }

  val runCountWords =
    ZIO
      .foreachPar(1 to 10) { i =>
        countWords(s"src/main/resources/test$i.txt").debugThread
      }
      .debug

  def run = chainedFibers.debugThread

// chainedFibers.debug
//    zippedFibers.debug
// runOnAnotherThread(meaningOfLife).debugThread
