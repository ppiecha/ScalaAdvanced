package applicative

trait Applicative[F[_]] {
   def pure[A](a: A): F[A]
   def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]
   def map[A, B](fa: F[A])(fab: A => B): F[B] = ap(pure(fab))(fa)
   def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ap(map(fa)(a => (b: B) => (a, b)))(fb)
   def map2[A, B, C](fabc: (A, B) => C)(fa: F[A], fb: F[B]): F[C] = map(product(fa, fb))(fabc.tupled)
   //def ap2[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2((a, b): (A, B) => b)(fa, fab)
   val f: = a => b => b
}
