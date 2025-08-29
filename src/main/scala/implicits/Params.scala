package implicits

import scala.language.implicitConversions

object Params extends App {
  case class Person(name: String, age: Int) {
    def say = s"Hello from $name"
  }
  implicit def stringToPerson(s: String): Person = Person(s, 1)
  println("Ania".say)
  given Ordering[Person] = Ordering.fromLessThan(_.name < _.name)
  println(List(Person("Ania", 1), Person("Piotr", 2), Person("Basia", 3)).sorted)

  case class Purchase(units: Int, unitPrice: Double) {
    private def totalPrice: Double = units * unitPrice
  }

  object Purchase {
    implicit val default: Ordering[Purchase] = Ordering.fromLessThan(_.totalPrice < _.totalPrice)
  }

  object PurchaseOrderedByCount {
    implicit val default1: Ordering[Purchase] = Ordering.fromLessThan(_.units < _.units)
  }

  object PurchaseOrderedByPrice {
    implicit val default2: Ordering[Purchase] = Ordering.fromLessThan(_.unitPrice < _.unitPrice)
  }
  //import PurchaseOrderedByCount._
  import PurchaseOrderedByPrice._
  println(List(Purchase(1, 3), Purchase(2, 1.0), Purchase(4, 1.0)).sorted)

  val res0 = 2 match {
    case 0 =>
    case 1 => ()
    case _ => println("")
  }

  case class User(name: String, email: String)

  trait Equal[T] {
    def equal(t1: T, t2: T): Boolean
  }

  implicit object EqualByName extends Equal[User] {
    override def equal(t1: User, t2: User): Boolean = t1.name == t2.name
  }

  object EqualByNameAndEmail extends Equal[User] {
    override def equal(t1: User, t2: User): Boolean = t1.name == t2.name && t1.email == t2.email
  }

  object Equal {
    def apply[T](t1: T, t2: T)(implicit instance: Equal[T]): Boolean = instance.equal(t1, t2)
  }

  implicit class UserEnrichment[T](t: T) {
    def ===(anotherValue: T)(implicit evidence: Equal[T]): Boolean = evidence.equal(t, anotherValue)
    def !==(anotherValue: T)(implicit evidence: Equal[T]): Boolean = ! ===(anotherValue)
  }

  val u1 = User("Piotr", "p")
  val u2 = User("Ania", "a")

  println(Equal(u1, u2))
  println(u1===u2)
  println(u1!==u2)
}
