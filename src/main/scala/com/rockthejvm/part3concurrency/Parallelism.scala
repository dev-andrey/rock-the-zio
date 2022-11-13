package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.debugThread

import java.io.File
import scala.io.Source

object Parallelism extends ZIOAppDefault:
  val meaningOfLife = ZIO.succeed(42L)
  val favLang       = ZIO.succeed("Scala")
  val combined      = meaningOfLife zip favLang // combines/zips sequential

  // combine in parallel
  val combinePar = meaningOfLife zipPar favLang // combines/zips in parallel

  /*
    - start each eff on fibers
    - in one fails? the other should be interrupted
    - what if one is interrupted? the entire thing should be interrupted
    - what if the whole thing is interrupted? need to interrupt both effects
   */

  // try zipPar combinator
  // hint: fork/join/await, interrupt

  def myZipPar[R, E, A, B](zioA: ZIO[R, E, A], zioB: ZIO[R, E, B]): ZIO[R, E, (A, B)] = {
    val exits = for
      fibA  <- zioA.fork
      fibB  <- zioB.fork
      exitA <- fibA.await
      exitB <- exitA match
                 case Exit.Success(a) => fibB.await
                 case Exit.Failure(_) => fibB.interrupt
    yield (exitA, exitB)

    exits.flatMap {
      case (Exit.Success(a), Exit.Success(b))           => ZIO.succeed((a, b))
      case (Exit.Success(_), Exit.Failure(cause))       => ZIO.failCause(cause) // zioA failed
      case (Exit.Failure(cause), Exit.Success(_))       => ZIO.failCause(cause) // zioB failed
      case (Exit.Failure(causeA), Exit.Failure(causeB)) => ZIO.failCause(causeA && causeB)
    }
  }

  val effects: Seq[UIO[Int]]         = (1 to 10).map(int => ZIO.succeed(int).debugThread)
  val collectedValues: UIO[Seq[Int]] = ZIO.collectAllPar(effects) // preserves order

  val printlnParallel = ZIO.foreachPar((1 to 10).toList)(int => ZIO.debug(int))

  val sumPar    = ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _)
  val sumPar_v2 = ZIO.mergeAllPar(effects)(0)(_ + _) // more generic

  /*
    Some RULES:
      - if all succeed, all good
      - if one failed => everything is interrupted, error is surfaced
      - if one is interrupted => everyone is interrupted, error = interruption
      - if the entire thing is interrupted => all effects are interrupted
   */

  // count all in parallel
  def countWords(path: String) =
    ZIO.fromAutoCloseable(ZIO.attempt(Source.fromFile(new File(path)))).map { source =>
      source.getLines().mkString(" ").split(' ').count(_.nonEmpty)
    }

  val runCountWords =
    ZIO
      .reduceAllPar(
        ZIO.succeed(0),
        (1 to 10).map { i =>
          countWords(s"src/main/resources/test$i.txt").debugThread
        }
      )(_ + _)

  def run = runCountWords.debugThread
