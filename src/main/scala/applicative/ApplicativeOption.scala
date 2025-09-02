package applicative

import cats._

object ApplicativeOption {
  val applicativeOption: Applicative[Option] = new Applicative[Option] {
    override def pure[A](x: A): Option[A] = Option(x)
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = (ff, fa) match {
      case (Some(f), Some(value)) => Some(f(value))
      case _ => None
    }
  }
}
