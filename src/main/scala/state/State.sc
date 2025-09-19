import cats._
import cats.implicits._
import cats.data._

// State S => (S, A)

val s = State[Int, String](i => (i + 1, "INT" + i.toString))
s.run(1).value

type St[A] = State[Int, A]

8.pure[St]

val x = State[Int, Double]{i => println(s"x $i"); (i + 1, (i + 11).toDouble)}
x.run(1).value
x.runS(1).value
x.runA(1).value

def get[S]: State[S, S] = State(s => (s, s))
def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
def inspect[S, T](f: S => T): State[S, T] = State(s => (s, f(s)))

val y = State[Int, Double]{i => println(s"y $i"); (i + 1, i / 3.0)}
x.run(3).value
y.run(3).value

x.flatMap(d => State(n => (n, d * 2))).run(1).value
(x, y).mapN((d1, d2) => d1 + d2).run(3).value