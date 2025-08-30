package catsMonoid

import cats._
import cats.implicits._

case class Speed(metersPerSecond: Double)

object Speed {
  def addSpeeds(speed1: Speed, speed2: Speed) = Speed(speed1.metersPerSecond + speed2.metersPerSecond)
  implicit val speedMonoid: Monoid[Speed] = Monoid.instance(Speed(0), (x, y) => Speed(x.metersPerSecond + y.metersPerSecond))
}

object TestSpeed extends App {
  val s1 = Speed(2.2)
  val s2 = Speed(2.8)
  assert(Speed.addSpeeds(s1, s2).metersPerSecond === 5)
  println(s1 |+| s2)
  println(Monoid[Speed].combineAll(Seq(s1, s2)))
  val sumMonoid: Monoid[Int] = Monoid.instance(0, _ + _)
  val minMonoid: Monoid[Int] = Monoid.instance(Int.MaxValue, _ min _)
  def listMonoid[A]: Monoid[List[A]] = Monoid.instance(List(), _ ++ _)
  val stringMonoid: Monoid[String] = Monoid.instance("", _ ++ _)
}
