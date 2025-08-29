package monad

trait Lazy[A] {
  def flatMap[B](f: A => Lazy[B]): Lazy[B]
  def use: A
}

case class LazyExpr[A](expr: () => A) extends Lazy[A] {
  def flatMap[B](f: A => Lazy[B]): Lazy[B] = f(expr())
  def use: A = expr()
}

object Lazy {
  def apply[A](value: A): Lazy[A] = LazyExpr(() => value)
}

object TestLazy extends App {
  val test1 = Lazy {
    println("Do nothing just print")
    42
  }
  val flatMapped1 = test1.flatMap(x => Lazy(x * 10))
  val flatMapped2 = test1.flatMap(x => Lazy(x * 10))

  flatMapped1.use
  flatMapped2.use
}
