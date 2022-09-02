package com.rockthejvm.part1recap

import scala.language.implicitConversions

object ContextualAbstractionScala2 {

  // Implicit classes

  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet(): String = Person(name).greet()
  }

  // extension method
  val greeting = "Peter".greet() // new ImpersonableString("Peter").greet()

  // example: scala.concurrent.duration
  import scala.concurrent.duration._
  val oneSecond = 1.second

  // Implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount
  def multiply(x: Int)(implicit factor: Int)  = x * factor

  implicit val defaultAmount: Int = 10

  val twelve   = increment(2) // implicit argument 10 passed by the compiler
  val aHundred = multiply(10) // same implicit argument 10 passed by the compiler

  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  def convert2Json[T](value: T)(implicit serializer: JsonSerializer[T]): String =
    serializer.toJson(value)

  implicit val personSerializer: JsonSerializer[Person] = new JsonSerializer[Person] {
    override def toJson(person: Person) = "{\"name\" : \"" + person.name + "\"}"
  }

  val davidsJson = convert2Json(Person("David"))

  // implicit defs
  implicit def createListSerializer[T](implicit serializer: JsonSerializer[T]): JsonSerializer[List[T]] =
    new JsonSerializer[List[T]] {
      override def toJson(list: List[T]) = s"[${list.map(serializer.toJson).mkString(",")}]"
    }

  val personsJson = convert2Json(List(Person("Alice"), Person("Bob")))

  // implicit conversions (not recommended)
  case class Cat(name: String) {
    def meow(): String = s"$name is meowing"
  }

  implicit def string2Cat(name: String): Cat = Cat(name)
  val aCat: Cat                              = "Garfield" // string2Cat("Garfield")
  val garfieldMeowing                        = "Garfield".meow()

  def main(args: Array[String]): Unit = {
    println(davidsJson)
    println(personsJson)
  }

}
