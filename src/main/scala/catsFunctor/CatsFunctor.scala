package catsFunctor

import cats._

object CatsFunctor extends App {

  val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case Some(value) => Some(f(value))
      case None => None
    }
  }

  val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa match {
      case ::(head, next) => f(head) :: map(next)(f)
      case Nil => Nil
    }
  }

  println(listFunctor.map(List(1,2,3))(_ * 2))

}
