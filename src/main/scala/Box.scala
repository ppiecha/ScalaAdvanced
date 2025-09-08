import cats._
import cats.implicits._

case class Box[A](value: A)
object Box extends App {
  def eq[A: Eq]: Eq[Box[A]] = Eq.by(_.value)
  given Monad[Box] = new Monad[Box] {
    override def pure[A](x: A): Box[A] = Box(x)
    override def flatMap[A, B](fa: Box[A])(f: A => Box[B]): Box[B] = f(fa.value)
    override def tailRecM[A, B](a: A)(f: A => Box[Either[A, B]]): Box[B] = f(a).value match {
      case Left(a) => tailRecM(a)(f)
      case Right(b) => Box(b)
    }
  }
}
