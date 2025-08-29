package catsEq

import cats._
import cats.implicits._

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  implicit val universalEq: Eq[Account] = Eq.fromUniversalEquals
  object Instances {
    implicit def numEq(implicit eqString: Eq[String]): Eq[Account] =
      (a1, a2) => eqString.eqv(a1.number, a2.number)
    implicit def numEq2: Eq[Account] = Eq.by(_.number)
  }
}

object TestAccountEq extends App {
  import Account.Instances.numEq2
  val a1 = Account(1, "000-1", 3.14, "me")
  val a2 = Account(2, "000-1", 3.15, "me2")
  println(a1 === a2)
}
