package catsShow

import cats._
import cats.implicits._

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  given toStringShow: Show[Account] = Show.fromToString
  object Instances {
    given ownerShow: Show[Account] = Show.show(_.owner)
    given constShow: Show[Account] = Show.show(a => "This account belongs to me")
  }
}

object TestShow extends App {
  import Account.Instances.ownerShow
  val a1 = Account(2, "000-1", 3.14, "me")
  val a2 = Account(1, "000-1", 3.15, "me2")
  assert(a1.show === "me")
}
