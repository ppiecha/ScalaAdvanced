import $dep. `org.typelevel::cats-core:2.13.0`
//"org.typelevel" %% "cats-core" % "2.13.0"

import cats.*
import cats.data.*
import cats.implicits.*

sealed trait IO[+A] {
    def resume: Either[() => IO[A], A] = this match
        case IO.Done(a) => Right(a)
        case IO.More(thunk) => thunk().resume
        case IO.FlatMap(ioa, faiob) => ioa match // ioa: IO[A] faiob: A => IO[B]
            case IO.Done(a2) => faiob(a2).resume
            case IO.More(thunk2) => Left(() => IO.FlatMap(thunk2(), faiob))
            case IO.FlatMap(ioc, fcioa) => IO.FlatMap(ioc, c => IO.FlatMap(fcioa(c), faiob)).resume // ioc: IO[C] fcioa: C => IO[A]
        
    def run: A = resume match
        case Left(thunk) => thunk().run
        case Right(a) => a
    
}

object IO {
    case class Done[A](a: A) extends IO[A]
    case class More[A](thunk: () => IO[A]) extends IO[A]
    case class FlatMap[A, B](ioa: IO[A], f: A => IO[B]) extends IO[B]

    def suspend[A](a: => A): IO[A] = More(() => Done(a))

    given Monad[IO] with {
        override def pure[A](a: A) = Done(a)
        override def flatMap[A, B](ioa: IO[A])(f: A => IO[B]): IO[B] = FlatMap(ioa, f)
        override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = ???
    }
}