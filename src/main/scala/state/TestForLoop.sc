import cats._
import cats.implicits._
import cats.data._

case class Player(name: String, age: Int)
val s1: State[Int, Int] = State(i => (i + 1, i))
val p1: State[Int, Player] = State(i => (i + 1, Player("Piotr", 44)))

val res = for {
  i <- s1
  p <- p1
} yield i
