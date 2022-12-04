package com.rockthejvm.part5testing

import com.rockthejvm.part5testing.MyBusinessApp.{normalizeUsername, Database}
import zio.*
import zio.test.*

object SimpleDependencySpec extends ZIOSpecDefault:
  def spec =
    suite("deps")(
      test("simple deps") {
        val aZIO: ZIO[Long, Nothing, Long] = ZIO.succeed(42L)
        assertZIO(aZIO.provideLayer(ZLayer.succeed(42L)))(Assertion.equalTo(42L))
      },
      test("simple deps 2") {
        val aZIO: ZIO[Long, Nothing, Long] = ZIO.succeed(42L)
        assertZIO(aZIO)(Assertion.equalTo(42L))
      }.provideLayer(ZLayer.succeed(42L))
    )

object MyBusinessApp:
  abstract class Database[K, V]:
    def get(key: K): Task[V]
    def put(key: K, value: V): Task[Unit]

  object Database:
    def create(url: String): UIO[Database[String, String]] = ???

  // business logic
  def normalizeUsername(name: String): UIO[String] = ZIO.succeed(name.toUpperCase)
object BusinessLogicSpec extends ZIOSpecDefault:
  private val mockedDatabase = ZIO.succeed(new Database[String, String]:
    import scala.collection.mutable
    val map = mutable.Map[String, String]()

    override def get(key: String)                = ZIO.attempt(map(key))
    override def put(key: String, value: String) = ZIO.attempt(map += (key -> value))
  )

  def spec =
    suite("user survey app")(
      test("normalize username") {
        val surveyLogic =
          for
            db         <- ZIO.service[Database[String, String]]
            _          <- db.put("123", "user")
            username   <- db.get("123")
            normalized <- normalizeUsername(username)
          yield normalized
        assertZIO(surveyLogic)(Assertion.equalTo("USER"))
      }
    ).provideLayer(ZLayer.fromZIO(mockedDatabase))

object BuiltInTestServiceSpec extends ZIOSpecDefault:
  def spec =
    suite("default services")(
      test("console") {
        val program =
          for
            name <- Console.readLine("enter name")
            _    <- Console.printLine(s"hello $name")
          yield ()

        val testing =
          for
            _      <- TestConsole.feedLines("alice")
            _      <- program
            output <- TestConsole.output
          yield output.map(_.trim)

        assertZIO(testing)(Assertion.hasSameElements(Seq("enter name", "hello alice")))
      },
      test("clock") {
        val program =
          for
            fiber  <- ZIO.sleep(5.minutes).timeout(1.minute).fork
            _      <- TestClock.adjust(1.minute)
            result <- fiber.join
          yield result

        assertZIO(program)(Assertion.isNone)
      },
      test("random") {
        val effect =
          for
            _     <- TestRandom.feedInts(3, 4, 1, 2)
            value <- Random.nextInt
          yield value

        assertZIO(effect)(Assertion.equalTo(3))
      }
    )
