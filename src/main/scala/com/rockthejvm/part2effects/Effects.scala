package com.rockthejvm.part2effects

import java.time.{LocalDateTime, ZoneOffset, ZonedDateTime}
import scala.concurrent.Future
import scala.util.Try

object Effects:

  // functional programming

  // local reasoning = type signature describes the kind of computation that will be performed
  def combine(a: Int, b: Int): Int = a + b

  // referential transparency = ability to replace an expression with the value it evaluates to
  val five    = combine(2, 3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  // not all expressions are referentially transparent
  // example 1: printing
  val resultOfPrinting: Unit    = println("Learning ZIO")
  val resultOfPrinting_v2: Unit = () // not the same program

  // example 2: changing a variable
  var anInt                = 0
  val changingInt: Unit    = anInt = 42 // side effect
  val changingInt_v2: Unit = ()         // not the same program

  // side effects are inevitable
  /*
    Effect properties (desires)
      - the type signature describes what kind of computation it will perform
      - the type signature describes the type of VALUE that it will produce
      - if side effects are required, construction must be separate from the EXECUTION
   */

  /*
    Example: Option = possibly absent values
      - type signature describes the kind of computation = a possibly absent value
      - type signature says that the computation returns an A, if the computation does produce something
      - no side effects are needed

    => Option is an effect
   */
  val anOption: Option[Int] = Option(42)

  /*
    Example 2: Future
      - describes an asynchronous computation
      - produces a value of type Int, if it finishes and it's successful
      - side effects are required, construction is NOT separate from execution
   */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)

  /*
    Example 3: MyIO
      - describes a computation which might perform side effects (including those performing side effects)
      - produces values of type A if the computation is successful
      - side effects are required, construction IS SEPARATE from execution

    MyIO is an effect
   */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIOWithSideEffects: MyIO[Int] = MyIO { () =>
    println("producing effect")
    42
  }

  /** Exercises - create some IO which
    *
    *   1. measure the current time of the system
    *
    * 2. measure the duration of a computation
    *   - use exercise 1
    *   - use map/flatMap combination of MyIO
    *
    * 3. read something from the console
    *
    * 4. print something to the console (e.g. "whats your name"), then read, then print a welcome message
    */

  // 1
  val currentTime = MyIO(() => System.currentTimeMillis())

  // 2
  def measure[A](computation: MyIO[A]): MyIO[(Long, A)] =
    for {
      before <- currentTime
      result <- computation
      after  <- currentTime
      diff    = after - before
    } yield (diff, result)

  // 3
  val readLine                = MyIO(() => Console.in.readLine())
  def writeLine(text: String) = MyIO(() => Console.out.println(text))

  val program = for {
    _    <- writeLine("what's your name?")
    name <- readLine
    _    <- writeLine(s"hello $name")
  } yield ()

  /** A simplified ZIO
    */
  case class MyZIO[-R, +E, +A](unsafeRun: R => Either[E, A]) {
    def map[B](f: A => B): MyZIO[R, E, B] =
      MyZIO { r =>
        unsafeRun(r) match
          case Left(e)  => Left(e)
          case Right(v) => Right(f(v))
      }

    def flatMap[R1 <: R, E1 >: E, B](f: A => MyZIO[R1, E1, B]): MyZIO[R1, E1, B] =
      MyZIO { r =>
        unsafeRun(r) match
          case Left(e)  => Left(e)
          case Right(v) => f(v).unsafeRun(r)
      }
  }

  def main(args: Array[String]): Unit =
    anIOWithSideEffects.unsafeRun()
