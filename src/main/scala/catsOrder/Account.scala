package catsOrder

import cats._
import cats.implicits._

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  implicit val orderById: Order[Account] = Order.from((a1, a2) => Order[Long].compare(a1.id, a2.id))
  object Instances {
    implicit val orderByBalance: Order[Account] = (a1, a2) => Order[Double].compare(a1.balance, a2.balance)
    implicit val orderByBalance2: Order[Account] = Order.by(_.balance)
  }
}

object TestAccountEq extends App {
  val a1 = Account(2, "000-1", 3.14, "me")
  val a2 = Account(1, "000-1", 3.15, "me2")
  assert(a2 <= a1)
  def catsSort[A](list: List[A])(implicit orderA: Order[A]): List[A] = list.sorted(orderA.toOrdering)
  val l = List(a1, a2)
  println(catsSort(l))
  import Account.Instances.orderByBalance2
  println(catsSort(l))
}
