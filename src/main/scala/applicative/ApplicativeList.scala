package applicative

object ApplicativeList {
  val applicativeList = new Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def ap[A, B](ff: List[A => B])(aa: List[A]): List[B] = (ff, aa) match {
      case (fh :: ft, h :: t) => (h :: t).map(fh) ++ ap(ft)(h :: t)
      case _ => Nil
    }
  }
}
