package monad

trait Monad[A] {
  //def unit(x: A): Monad[A]
  def flatMap[B](f: A => Monad[B]): Monad[B]
  def map[B](f: A => B): Monad[B] = flatMap(Monad.apply[B] compose f)
  def flatten(value: Monad[Monad[A]]): Monad[A] = flatMap(Monad.apply[A])
}

object Monad {
  def apply[A](value: A): Monad[A] = ???
}
