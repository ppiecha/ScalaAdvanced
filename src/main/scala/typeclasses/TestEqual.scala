package typeclasses
object TestEqual extends App {

  trait Equal[A] {
    def equal(a1: A, a2: A): Boolean
  }

//  object Equal {
//    implicit object byName extends Equal[Person] {
//      override def equal(a1: Person, a2: Person): Boolean = a1.name == a2.name
//    }
//  }

  class Person(val name: String, val age: Int)
  object Person {
    implicit val byAge: Equal[Person] = (a1: Person, a2: Person) => a1.age == a2.age
  }

  implicit class Ops[A](value: A)(implicit ev: Equal[A]) {
    def ===(other: A): Boolean = ev.equal(value, other)
  }

  //import Person._
  val p = Person("Piotr", 44)
  val a = Person("Ania", 44)
  println(a == p)
  println(implicitly[Equal[Person]])
  println(a === p)
}
