package typeclasses

import scala.annotation.tailrec

object Pimp extends App {

  println("1".toInt )
  implicit class RichString(s: String) {
    def asInt: Int = s.toInt
    def enrypt = ???
  }

  implicit class RichInt(i: Int) {
    def times[B](f: () => B): Unit = {
      @tailrec
      def loop(f: () => B, n: Int): Unit = {
        f()
        loop(f, n - 1)
      }
      loop(f, i)
    }
    //override def *(list: List)
  }

}
