package com.rockthejvm.part5testing
import zio.*
import zio.test.*

final case class Person(name: String, age: Int):
  def spellName    = name.toUpperCase
  def saySomething = ZIO.succeed(s"Hi, I'm $name")

object MyTestSpec extends ZIOSpecDefault:
  def spec =
    suite("person suite")(
      test("first test") {
        val person = Person("John", 33)
        assert(person.spellName)(Assertion.equalTo("JOHN")) &&
        assertTrue(person.spellName == "JOHN")
      },
      test("second test") {
        val person = Person("John", 44)
        assertZIO(person.saySomething)(Assertion.equalTo(s"Hi, I'm John"))
      },
      test("test for failure") {
        val eff = ZIO.fail("my bad")
        assertZIO(eff.exit)(Assertion.fails(Assertion.equalTo("my bad")))
      },
      test("test for death") {
        val eff = ZIO.attempt(42 / 0).orDie
        assertZIO(eff.exit)(Assertion.diesWithA[ArithmeticException])
      },
      suite("a sub-suite")(
        test("a nested test") {
          assert(List(1, 2, 3))(Assertion.isNonEmpty)
        }
      )
    )
