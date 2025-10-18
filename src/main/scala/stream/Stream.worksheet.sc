import $dep. `org.typelevel::cats-core:2.13.0`
//"org.typelevel" %% "cats-core" % "2.13.0"

import cats.*
import cats.data.*
import cats.implicits.*

case class Stream[+A](head: A, tail: Eval[Stream[A]]) {
    def take(n: Int): Eval[List[A]] = if n == 1 then Eval.now(List(head)) else tail.flatMap(t => t.take(n - 1).map(tl => head :: tl))
}
object Stream {
    def iterate[A](initial: A)(f: A => A): Stream[A] = Stream(initial, Eval.later(iterate(f(initial))(f)))
    given Functor[Stream] = new Functor[Stream] {
        def map[A, B](fa: Stream[A])(f: A => B): Stream[B] = Stream(f(fa.head), fa.tail.map(t => map(t)(f)))
    }
}

val ints = Stream.iterate(1)(i => i + 1)
ints.take(5).value
Functor[Stream].map(ints)(n => (n, n % 2)).take(10).value
ints.map(n => n * 2).take(5).value

def iterateWhileM[A](initial: A)(f: A => Option[A])(p: A => Boolean): Option[A] = {
    val value = f(initial)
    if p(initial) then value.flatMap(a => iterateWhileM(a)(f)(p)) else Some(initial)
}