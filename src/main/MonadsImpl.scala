

object MonoidImpl {

  /* The names functor and monad come from the branch of mathematics called category theory
  * */

  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
  }



  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
    def map[A,B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
    Since Monad provides a default implementation of map, it can extend Functor. All monads are functors, but not all functors are monads.
    def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
  }



}

