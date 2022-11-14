package com.rockthejvm.part3concurrency
import zio.*
import com.rockthejvm.utils.*

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
object AsyncEffects extends ZIOAppDefault:

  // async api - callback based
  object LoginService {
    case class AuthError(message: String)
    case class UserProfile(email: String, name: String)

    // thread pool
    val executor = Executors.newFixedThreadPool(4)

    // "database"
    val passwords = Map(
      "test@example.com" -> "pass1!"
    )

    val database = Map(
      "test@example.com" -> "John Doe"
    )

    def login(email: String, password: String)(onSuccess: UserProfile => Unit, onFailure: AuthError => Unit) =
      executor.execute { () =>
        println(s"[${Thread.currentThread().getName}] attempting login for $email")
        passwords.get(email) match
          case Some(pwd) if pwd == password => onSuccess(UserProfile(email, database(email)))
          case Some(_)                      => onFailure(AuthError("Incorrect password."))
          case None                         => onFailure(AuthError(s"User ${email} doesn't exist"))
      }
  }

  def loginAsZIO(id: String, pw: String) =
    ZIO.async[Any, LoginService.AuthError, LoginService.UserProfile] { cb =>
      LoginService.login(id, pw)(
        profile => cb(ZIO.succeed(profile)), // notify ZIO fiber to complete the ZIO with a success
        error => cb(ZIO.fail(error))         // same, with a failure
      )
    }

  val loginProgram =
    for
      email   <- Console.readLine("email: ")
      pass    <- Console.readLine("password: ")
      profile <- loginAsZIO(email, pass).debugThread
      _       <- Console.printLine(s"Welcome ${profile.name}")
    yield ()

  /** Exercises
    */
  // 1 - surface a computation running on some (external) thread to ZIO
  def external2ZIO[A](computation: () => A)(executor: ExecutorService): Task[A] =
    ZIO.async[Any, Throwable, A] { cb =>
      executor.execute { () =>
        try {
          val a = computation()
          cb(ZIO.succeed(a))
        } catch case ex: Throwable => cb(ZIO.fail(new RuntimeException("problem")))
      }
    }

  val demoExternal2ZIO = {
    val executor = Executors.newFixedThreadPool(4)
    val zio      = external2ZIO { () =>
      println(s"[${Thread.currentThread().getName}] computing the meaning of life on some thread")
      Thread.sleep(1000)
      42
    }(executor)

    zio.debugThread.unit
  }

  // 2 - lift a Future into a ZIO
  def futureToZIO[A](future: => Future[A])(implicit ec: ExecutionContext): Task[A] =
    ZIO.async { cb =>
      future.onComplete {
        case Failure(err)   => cb(ZIO.fail(err))
        case Success(value) => cb(ZIO.succeed(value))
      }
    }

  lazy val demoFutureToZIO = {
    val executor               = Executors.newFixedThreadPool(4)
    given ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

    val mol = futureToZIO(Future {
      println(s"[${Thread.currentThread().getName}] computing the meaning of life on some thread")
      Thread.sleep(1000)
      42
    })

    mol.debugThread.unit
  }

  // 3 - implement never-ending zio
  def neverEndingZIO[A]: UIO[A] = ZIO.async(_ => ())

  def run = demoFutureToZIO
