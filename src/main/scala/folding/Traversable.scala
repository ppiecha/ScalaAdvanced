package folding

import cats._
import cats.implicits._

object Traversable extends App {
  given t: Traverse[MList] = new Traverse[MList] {
    override def foldLeft[A, B](fa: MList[A], b: B)(f: (B, A) => B): B = fa match {
      case MCons(hd, tl) => f(foldLeft(tl, b)(f), hd)
      case MNil => b
    }
    override def foldRight[A, B](fa: MList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case MCons(hd, tl) => f(hd, foldRight(tl, lb)(f))
      case MNil => lb
    }
    override def traverse[G[_] : Applicative, A, B](fa: MList[A])(f: A => G[B]): G[MList[B]] = fa match {
      case MCons(hd, tl) => (f(hd), traverse(tl)(f)).mapN(MCons.apply)
      case MNil => Applicative[G].pure(MNil)
    }
    def traverseBySequence[G[_] : Applicative, A, B](fa: MList[A])(f: A => G[B]): G[MList[B]] =
      sequence(MList.F.map(fa)(f))
    override def sequence[G[_], A](fga: MList[G[A]])(using G: Applicative[G]): G[MList[A]] = fga match {
      case MCons(hd, tl) => (hd, sequence(tl)).mapN(MCons.apply)
      case MNil => G.pure(MNil)
    }
    def sequence2[G[_], A](fga: MList[G[A]])(using G: Applicative[G]): G[MList[A]] =
      traverse(fga)(identity)
    }
  given Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = fa match {
      case Some(a) => Applicative[G].map(f(a))(Option.apply)
      case None => Applicative[G].pure(None)
    }
    override def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa match {
      case Some(a) => f(b, a)
      case None => b
    }
    override def foldRight[A, B](fa: Option[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case Some(a) => f(a, lb) 
      case None => lb
    }
  }
  
}


