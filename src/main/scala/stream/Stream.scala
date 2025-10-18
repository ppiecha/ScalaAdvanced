package stream

import cats.*
import cats.data.*
import cats.implicits.*

case class Stream[+A](head: A, tail: Eval[Stream[A]]) {
    def take(n: Int): Eval[List[A]] = if n == 1 then Eval.now(List(head)) else tail.flatMap(t => t.take(n - 1).map(tl => head :: tl))
}
object Stream {
    def iterate[A](initial: A)(f: A => A): Stream[A] = Stream(initial, Eval.later(iterate(f(initial))(f)))
}

object TestStream extends App {
  
    val res0 = Stream.iterate(1)(i => i + 1)
    println(res0.take(5).value)

}