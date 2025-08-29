package typeclasses

trait Show[A] {
  def show(a: A): String
}

object Show {
  def apply[A](a: A)(implicit ev: Show[A]): String = ev.show(a)
  implicit val personInstance: Show[Person] = p => s"name: ${p.name} age: ${p.age}"
  implicit val intInstance: Show[Int] = i => s"value: ${i.toString}"
}

class Person(val name: String, val age: Int)

implicit class ShowOps[A](value: A)(implicit ev: Show[A]) {
  def show: String = ev.show(value)
}

object TestShow extends App {
  val p = Person(name="Piotr", age=44)
  println(p.show)
  println(1.show)
  println(Show(10))
}
