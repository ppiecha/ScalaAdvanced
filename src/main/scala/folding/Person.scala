package folding

import cats._
import cats.implicits._

case class Person(name: String)
object Person extends App {
  def findPersonByName(list: MList[Person], name: String)(using f: Foldable[MList]): Option[Person] =
    f.find(list)(_.name == name)
  def findPeopleByNames(list: MList[Person], names: MList[String])(using f: Foldable[MList]): Option[MList[Person]] =
    if f.forall(list)(p => names.contains_(p.name)) then Some(list) else None
  def traverse[F[_]: Applicative, A, B](as: MList[A])(f: A => F[B]): F[MList[B]] = as match {
    case MCons(hd, tl) => (f(hd), traverse(tl)(f)).mapN(MCons.apply)
    case MNil => Applicative[F].pure(MNil)
  }
  println(findPeopleByNames(MList(Person("1"), Person("2"), Person("3")), MList("1", "2", "3")))
  println(findPeopleByNames(MList(Person("1"), Person("2"), Person("3")), MList("1", "3")))
}
