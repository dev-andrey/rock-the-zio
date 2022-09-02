package com.rockthejvm.part2effects

import zio.*

import java.io.IOException
import java.net.NoRouteToHostException

object ZioErrorHandling extends ZIOAppDefault:

  // ZIO can fail
  val aFailedZIO            = ZIO.fail("Something went wrong")
  val failedWithThrowable   = ZIO.fail(new RuntimeException("boom"))
  val failedWithDescription = failedWithThrowable.mapError(_.getMessage)

  // attempt: run an affect that might through an exception

  val badZIO = ZIO.succeed {
    println("doing bad stuff")
    val string: String = null
    string.length
  }

  val anAttempt = ZIO.attempt {
    println("doing bad stuff")
    val string: String = null
    string.length
  }

  // effectfully catch errors
  val catchError           = anAttempt.catchAll(e => ZIO.succeed(s"Returning a different value because $e"))
  val catchSelectiveErrors = anAttempt.catchSome {
    case e: RuntimeException => ZIO.succeed(s"Ignoring runtime: $e")
    case _                   => ZIO.succeed("Ignoring everything else")
  }
  // chain effects
  val aBetterAttempt       = anAttempt.orElse(ZIO.succeed(56))
  // fold: handle success and failure
  val handleBoth           = anAttempt.fold(ex => "Something bad happened: $ex", value => s"Length was ${value}")
  // effectful fold
  val handleBoth_v2        = anAttempt.foldZIO(
    ex => ZIO.succeed(s"something bad happened: $ex"),
    value => ZIO.succeed(s"size $value")
  )

  // Conversion between Option/Try/Either to ZIO
  val aTryToZIO = ZIO.fromTry(scala.util.Try(42 / 0))

  // Either -> ZIO
  val anEither: Right[Long, String]   = Right("Success")
  val anEitherToZIO: IO[Long, String] = ZIO.fromEither(anEither)

  // ZIO -> ZIO with Either
  val eitherZIO    = anAttempt.either
  // reverse
  val anAttempt_v2 = eitherZIO.absolve

  // option -> ZIO
  val anOption: IO[Option[Nothing], Long] = ZIO.fromOption(Some(42L))

  /*
    Errors   = failures presents in the ZIO type signature ("checked" exceptions)
    Defects  = failures that are unrecoverable, unforeseen, NOT present on Error channel

    ZIO[R,E,A] can finish with Exit[E, A]:
      - Success[A] containing value A
      - Fail with the Cause[E]
          - Fail[E] containing the error
          - Die(t: Throwable) which was unforeseen
   */
  val divisionByZIO: UIO[Int] = ZIO.succeed(1 / 0)

  val failedInt: ZIO[Any, String, Int]                  = ZIO.fail("I failed")
  val failureCauseExposed: ZIO[Any, Cause[String], Int] = failedInt.sandbox
  val failureCauseHidden: ZIO[Any, String, Int]         = failureCauseExposed.unsandbox

  // folded with cause
  val foldedWithCause = failedInt.foldCause(
    cause => s"this failed with ${cause.defects}",
    value => s"this succeeded with ${value}"
  )

  val foldedWithCauseZIO = failedInt.foldCauseZIO(
    cause => ZIO.debug(s"this failed with ${cause.defects}"),
    value => ZIO.debug(s"this succeeded with ${value}")
  )

  /** Good practice
    *
    *   - at a lower level, "errors" should be treated
    *   - at a higher level, you should hide "errors" and assume they are unrecoverable. UIO, URIO
    */
  def callHttpEndpoint(url: String): ZIO[Any, IOException, String] =
    ZIO.fail(new IOException("no internet, dummy!"))

  val endpointCallWithDefects: ZIO[Any, Nothing, String] =
    callHttpEndpoint("https://rockthejvm.com").orDie // consider Errors as Defects

      // refine the error channel
  def callHttpEndpointWideError(url: String): ZIO[Any, Exception, String] =
    ZIO.fail(new Exception("no internet, dummy!"))

  def refinedCallHttpEndpoint(url: String) =
    callHttpEndpointWideError("https://rockthejvm.com").refineOrDie[IOException] {
      case e: IOException            => e
      case _: NoRouteToHostException => new IOException(s"No routes to host to $url")
    }

  // reverse: turn defects into the error channel
  val endpointCallWithError = endpointCallWithDefects.unrefine { case e =>
    e.getMessage
  }

  /** Combine effects with different errors
    */

  // ex: web-crawler

  // errors coming from different modules
  case class IndexError(message: String)
  case class DbError(message: String)

  val callApi: ZIO[Any, IndexError, String] = ZIO.succeed("page: <html></html>")
  val queryDb: ZIO[Any, DbError, Long]      = ZIO.succeed(1L)

  val combined: ZIO[Any, IndexError | DbError, (String, Long)] = for
    page         <- callApi
    rowsAffected <- queryDb
  yield (page, rowsAffected) // Lost type-safety of Error channel ZIO[Any, Product, (String,Int)]

  /** Solutions:
    *   - design an error module for the whole app
    *   - scala3 union types
    *   - .mapError to some common error type
    */

  /** Exercises
    */
  // 1: make this effect fail with a TYPED error
  val aBadFailure = ZIO.succeed[Int](throw new RuntimeException("this is bad"))

  val solution1 = aBadFailure.sandbox              // expose defect in Cause
  val solution2 = aBadFailure.unrefine { case e => // surfaces the exception
    e
  }

  // 2: turn zio into zio with a narrower error type
  def ioException[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] =
    zio.refineToOrDie[IOException]

  // 3: transform zio
  def left[R, E, A, B](zio: ZIO[R, E, Either[A, B]]): ZIO[R, Either[E, A], B] =
    zio.foldZIO(
      e => ZIO.fail(Left(e)),
      _ match
        case Left(a)  => ZIO.fail(Right(a))
        case Right(b) => ZIO.succeed(b)
    )

  // 4:
  val database = Map(
    "daniel" -> 123,
    "alice"  -> 789
  )
  case class QueryError(reason: String)
  case class UserProfile(name: String, phone: Int)

  def lookupProfile(userId: String): ZIO[Any, QueryError, Option[UserProfile]] =
    if (userId != userId.toLowerCase)
      ZIO.fail(QueryError("user ID format is invalid"))
    else
      ZIO.succeed(database.get(userId).map(phone => UserProfile(userId, phone)))

  // surface out all the failed cases of this API
  enum AppError:
    case NotFound(msg: String)     extends AppError
    case InvalidInput(msg: String) extends AppError

  def betterLookupProfile(userId: String) =
    lookupProfile(userId).foldZIO(
      failure => ZIO.fail(AppError.InvalidInput(failure.reason)),
      _ match
        case None        => ZIO.fail(AppError.NotFound("User isn't found"))
        case Some(value) => ZIO.succeed(value)
    )

  val run = anAttempt

