package typeclasses

trait Combine[A] {
  def combine(a1: A, a2: A): A
  extension (a: A) {
    def +++(another: A): A = combine(a, another)
  }
}

object Combine {
  def apply[A](a1: A, a2: A)(using ev: Combine[A]): A = ev.combine(a1, a2)
  given Combine[String] = (a1, a2) => a1 + a2
}

object TestCombiner extends App {
  import Combine.given 
  println(Combine("hello", "world"))
  println("hello " +++ "world")
}
