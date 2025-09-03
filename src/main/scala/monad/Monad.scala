package monad

import scala.util.{Failure, Success, Try}

trait Monad[F[_]] {
  def unit[A](x: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(unit[B] compose f)
  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
}

object Monad extends App {
  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](x: A): List[A] = List(x)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa match {
      case ::(head, next) => f(head) ++ flatMap(next)(f)
      case Nil => Nil
    }
  }
  def eitherMonad[E]: Monad[[X] =>> Either[E, X]] = new Monad[[X] =>> Either[E, X]] {
    override def unit[A](x: A): Either[E, A] = Right(x)
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
      case Left(value) => Left(value)
      case Right(value) => f(value)
    }
  }
  val tryMonad: Monad[Try] = new Monad[Try] {
    override def unit[A](x: A): Try[A] = Success(x)
    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa match {
      case Failure(exception) => Failure(exception)
      case Success(value) => f(value)
    }
  }
  println(listMonad.flatMap(List(1, 3 ,5))(n => List(n, n + 1)))
  println(eitherMonad[String].flatMap(eitherMonad.unit(1))(n => if n < 2 then Left("Error") else Right(n * 2)))
  println(eitherMonad[String].flatMap(eitherMonad.unit(2))(n => if n < 2 then Left("Error") else Right(n * 2)))
  println(tryMonad.flatMap(tryMonad.unit(2))(n => Try(n + 2)))
}


