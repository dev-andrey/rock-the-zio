package com.rockthejvm.part1recap

object Variance {

  // OOP substitutions
  class Animal
  class Dog(name: String) extends Animal

  // Variance questions for List: if Dog <: Animal (subtype), should List[Dog] <: List[Animal]?

  // YES - COVARIANT
  val lassie = new Dog("Lassie")
  val hachi  = new Dog("Hachi")
  val laika  = new Dog("Laika")

  val anAnimal: Animal          = lassie
  val someAnimals: List[Animal] = List(lassie, hachi, laika)

  class MyList[+A] // MyList is COVARIANT in A
  val myAnimalList: MyList[Animal] = new MyList[Dog]

  // NO - then the type is INVARIANT
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  // NO - all generics in JAVA
  // val aJavaList: java.util.ArrayList[Animal] = new java.util.ArrayList[Dog]() - not possible

  // HELL NO - CONTRAVARIANCE
  trait Vet[-A] {
    def heal(animal: A): Boolean
  }

  // Vet[Animal] is "better" then a Vet[Dog] as it can treat all animals, therefore my dog as well
  // Dog <: Animal, then Vet[Dog] >: Vet[Animal]
  val myVet: Vet[Dog] = new Vet[Animal] {
    override def heal(animal: Animal) = {
      println("Here you go, you're good now")
      true
    }
  }

  val healingLassie = myVet.heal(lassie)

  /*
    Rule of thumb:
    - if the type PRODUCES or RETRIEVES values of type A (e.g. lists), then the type should be Covariant
    - if the type CONSUMES or ACTS ON values of type A (e.g. a Vet), then the type should be CONTRAVARIANT
    - otherwise, INVARIANT
   */

  /** Variance position
    */

  /*
    class Vet2[-A](val favoriteAnimal: A) <-- types of val fields are in COVARIANT position
    DOESN'T COMPILE, assuming it does;

    val garfield = new Cat
    val theVet: Vet2[Animal] = new Vet2[Animal](garfield)

    val dogVet: Vet2[Dog] = theVet
    val favAnimal:Dog = dogVet.favoriteAnimal // must be a Dog - type conflict!!
   */

  /*
    class MutableContainer[+A](var contents: A) <-- types of vars are in CONTRAVARIANT position

    val containerAnimal: MutableContainer[Animal] = new MutableContainer[Dog](new Dog)
    containerAnimal.contents = new Cat // type conflict!
   */

  // types of method arguments are in CONTRAVARIANT position
  class MyList2[+A] {
    def add[B >: A](element: B): MyList2[B] = ??? // B is supertype of A, type WIDEN
  }

  /* Because:
       val animals: MyList2[Animal] = new MyList2[Cat]
       val biggerListOfAnimals: MyList2[Animal] = animals.add(new Dog) // needs to be prevented
   */

  abstract class Vet2[-A] {
    // Production method, but Type is Contravariant in A
    def rescueAnimal[B <: A](): B // B is subtype of A
  }

  /* Because:
      val vet: Vet2[Animal] = new Vet2[Animal] {
        def rescueAnimal(): Animal = new Cat
      }
      val lassieVet: Vet2[Dog] = vet
      val rescueDog:Dog = lassieVet.rescueAnimal() // must return a Dog but returns a Cat

   */

  def main(args: Array[String]): Unit = {}

}
