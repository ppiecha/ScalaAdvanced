package validated

import cats._

enum Validated[+A] {
  case Valid(value: A)
  case Invalid(errors: List[String])
}

object Validated {
  given Applicative[Validated] with {
    def pure[A](a: A) = Valid(a)
    def ap[A, B](vf: Validated[A => B])(va: Validated[A]): Validated[B] = va match {
      case Validated.Valid(a) => vf match {
        case Validated.Valid(fab) => Valid(fab(a))
        case Validated.Invalid(errors) => Validated.Invalid(errors)
      }
      case Validated.Invalid(ae) => vf match {
        case Validated.Valid(f) => Invalid(ae)
        case Validated.Invalid(fe) => Invalid(ae ++ fe)
      }
    }
    def map[A, B](f: A => B)(va: Validated[A]): Validated[B] = ap(pure(f))(va)
    //def lift[A, B](f: A => B): Validated[A] => Validated[B] = va => map(f)(va)
    def product2[A, B](va: Validated[A], vb: Validated[B]): Validated[(A, B)] =
      ap(map((a: A) => (b: B) => (a, b))(va))(vb)
    def map2[A, B, C](f: (A, B) => C)(va: Validated[A], vb: Validated[B]): Validated[C] =
      map(f.tupled)(product(va, vb))
  }
}

