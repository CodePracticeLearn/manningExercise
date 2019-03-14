

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

//simple

  trait M[A] {
    def flatMap[B](f: A => M[B]): M[B]
  }

  def unit[A](x: A): M[A] //outside trait body /* * “Unit” is merely a convention for referencing monad’s identity operation in Scala/

  /**
    *  identity (return in Haskell, unit in Scala)
       bind (>>= in Haskell, flatMap in Scala) */

  /* we want to wrap some object with our monad wrapper, we must parameterize the monad with
  the type of the underlying object, e.g. M[Int], M[String], M[MyClass] etc*/


  /* Let’s say U is a List. It works for various other types, but we’ll use List for this example. Now, what flatMap
  does is that it takes a function with signature A → List[B] and it uses that function to transform the
  underlying object of type A into a List[B]. This operation is known as map. Since we transformed our underlying A into a List[B],
  this leaves us with a List[List[B]]. But we did not use ordinary map() — we used flatMap().
   This means that the job is not done yet; flatMap will now “flatten” our List[List[B]] into List[B].
    //link: https://medium.freecodecamp.org/demystifying-the-monad-in-scala-cc716bb6f534
*/


}

