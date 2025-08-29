package lazyPkg

import scala.annotation.tailrec

object MyStreamObj extends App {

  trait MyStream[+A] {

    def head: A

    def tail: () => MyStream[A]

    def isEmpty: Boolean

    def #::[B >: A](elem: B): MyStream[B]

    def ++[B >: A](anotherStream: MyStream[B]): MyStream[B]

    def foreach(f: A => Unit): Unit

    def map[B](f: A => B): MyStream[B]

    def flatMap[B](f: A => MyStream[B]): MyStream[B]

    def filter(predicate: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A]

    def takeAsList(n: Int): List[A]

  }

  object MyStream {
    def from[A](start: A)(gen: A => A): MyStream[A] = {
      //println("from " + start)
      NonEmptyStream(start, () => from(gen(start))(gen))
    }
  }


  case object EmptyStream extends MyStream[Nothing] {
    def head: Nothing = throw new RuntimeException("EmptyStream.head")

    def tail: () => MyStream[Nothing] = throw new RuntimeException("EmptyStream.tail")

    def isEmpty: Boolean = true

    def #::[B](elem: B): MyStream[B] = NonEmptyStream(elem, () => EmptyStream)

    def ++[B >: Nothing](anotherStream: MyStream[B]): MyStream[B] = anotherStream

    def foreach(f: Nothing => Unit): Unit = ()

    def map[B](f: Nothing => B): MyStream[B] = EmptyStream

    def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = EmptyStream

    def filter(predicate: Nothing => Boolean): MyStream[Nothing] = EmptyStream

    def take(n: Int): MyStream[Nothing] = EmptyStream

    def takeAsList(n: Int): List[Nothing] = List()

  }

  case class NonEmptyStream[A](override val head: A, override val tail: () => MyStream[A]) extends MyStream[A] {

    override def isEmpty: Boolean = false

    override def #::[B >: A](elem: B): MyStream[B] = NonEmptyStream(elem, () => this)

    override def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] = NonEmptyStream(head, () => tail() ++ anotherStream)

    override def foreach(f: A => Unit): Unit = {
      f(head)
      tail().foreach(f)
    }

    override def map[B](f: A => B): MyStream[B] = NonEmptyStream(f(head), () => tail().map(f))

    override def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail().flatMap(f)

    override def filter(predicate: A => Boolean): MyStream[A] = {
      val fTail = () => tail().filter(predicate)
      if predicate(head) then NonEmptyStream(head, fTail)
      else fTail()
    }

    override def take(n: Int): MyStream[A] = {
      if n <= 0 then EmptyStream
      else if n == 1 then NonEmptyStream(head, () => EmptyStream)
      else NonEmptyStream(head, () => tail().take(n - 1))
    }

    override def takeAsList(n: Int): List[A] = if n <= 0 || isEmpty then List() else head :: tail().takeAsList(n - 1)

  }

//  println("test")
//  val s1 = MyStream.from(1)(_ + 1)
//  val s2 = 0 #:: s1.take(3)
//  s2.foreach(n => "foreach: " + println(n))
//  val s3 = MyStream.from(1)(_ + 1)
//  val s4 = MyStream.from(1)(_ - 1)
//  val s5 = s3.take(3) ++ s4.take(3)
//  val s6 = s5.map(_ * 2)
//  val s7 = s5.filter(n => n % 2 == 0)
//  val s8 = s7.flatMap(MyStream.from(_)(_ - 1))
//  println(s5.takeAsList(6))
//  val s9 = MyStream.from(1)(_ + 1)
//  println(s9.head)
//  println(s9.tail().head)
//  println(s9.tail().tail().head)
//  println("end of test")

  //def fib(n: Int): Int = if n == 1 || n ==2 then 1 else fib(n-1) + fib(n-2)
  // 0 1 1 2 3 5 8 13
  //val fib: MyStream[Int] = 0 #:: 1 #:: NonEmptyStream(1, () => NonEmptyStream(fib.head, fib.head + fib.tail().head))
  val fib: MyStream[(Int, Int)] = MyStream.from((0, 1))((prev, next) => (next, next + prev))
  println(fib.takeAsList(10))

  def fibonacci(first: Int, second: Int): MyStream[Int] = NonEmptyStream(first, () => fibonacci(second, first + second))

  val fibStream = fibonacci(0, 1)

  println(fibStream.takeAsList(10))

}
