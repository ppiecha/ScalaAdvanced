package folding

import cats.{Eval, Foldable}
import cats.kernel.Monoid
import cats.implicits.*

import scala.annotation.tailrec

sealed trait MList[+A]
case class MCons[+A](hd: A, tl: MList[A]) extends MList[A]
case object MNil extends MList[Nothing]

object MList extends App {
  def apply[A](elems: A*): MList[A] = elems.foldRight(MNil: MList[A])(MCons(_, _))
  def sum(ints: MList[Int]): Int = {
    @tailrec
    def rec(ints: MList[Int], acc: Int = 0): Int = ints match {
      case MCons(hd, tl) => rec(tl, acc + hd)
      case MNil => acc
    }
    rec(ints)
  }
  def length(ints: MList[Int]): Int = ints match {
    case MCons(hd, tl) => 1 + length(tl)
    case MNil => 0
  }
  def filterPositive(ints: MList[Int]): MList[Int] = ints match {
    case MCons(hd, tl) => if hd > 0 then MCons(hd, filterPositive(tl)) else filterPositive(tl)
    case MNil => MNil
  }
  def foldRight[A, B](list: MList[A])(z: B)(f: (A, B) => B): B = list match {
    case MCons(hd, tl) => f(hd, foldRight(tl)(z)(f))
    case MNil => z
  }
  def foldLeft[A, B](list: MList[A])(z: B)(f: (B, A) => B): B = list match {
    case MCons(hd, tl) => foldLeft(tl)(f(z, hd))(f)
    case MNil => z
  }
  def foldMap[F[_], A, B](fa: F[A])(f: A => B)(implicit m: Monoid[B], foldable: Foldable[F]) = {
    foldable.foldLeft(fa, m.empty)((b, a) => m.combine(b, f(a)))
  }
  def sum2(ints: MList[Int]): Int = foldRight(ints)(0)(_ + _)
  def sum3(ints: MList[Int]): Int = foldLeft(ints)(0)(_ + _)
  def length2(ints: MList[Int]): Int = foldRight(ints)(0)((a, b) => b + 1)
  def filterPositive2(ints: MList[Int]): MList[Int] = foldRight[Int, MList[Int]](ints)(MNil)((a, b) => if a > 0 then MCons(a, b) else b)
  println(sum(MCons(1, MCons(2, MNil))))
  println(length(MCons(1, MCons(-2, MNil))))
  println(filterPositive(MCons(1, MCons(-2, MNil))))
  println(sum2(MCons(1, MCons(2, MNil))))
  println(length2(MCons(1, MCons(-2, MNil))))
  println(filterPositive2(MCons(1, MCons(-2, MNil))))
  println(sum3(MCons(1, MCons(2, MNil))))
  println(foldMap(List(1, 2, 3))(_.show))
  println(MList(1,2,3))
  println(Foldable[List].fold(List(1,2,3)))
  println(Foldable[List].fold(List("a","b","c")))
}

