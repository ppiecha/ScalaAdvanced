package folding

import cats._
import cats.implicits._

object Utils extends App {
  def find[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Option[A] =
    fa.foldLeft(None: Option[A])((opt, a) => if p(a) then Some(a) else opt)
  def exists[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Boolean =
    fa.foldLeft(false)((b, a) => if p(a) then true else b)
  def toList[F[_]: Foldable, A](fa: F[A]): MList[A] =
    fa.foldRight[MList[A]](Eval.now(MNil))((a, eb) => Eval.now(MCons(a, eb.value))).value
  def forall[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Boolean =
    fa.foldLeft(true)((b, a) => p(a) && b)

  println(find(List(1, 2, 3))(_ % 2 == 1))
  println(exists(List(1, 2, 3))(_ == 2))
  println(exists(List(1, 2, 3))(_ == 0))
  println(toList(List(1, 2, 3)))
  println(forall(List(1, 2, 3))(_ > 0))
  println(forall(List(1, 2, 3))(_ %  2 == 0))
}
