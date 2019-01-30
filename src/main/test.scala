import scala.annotation.tailrec

object test extends App {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, n: Int): Int = if (n > 0) go(b, a + b, n - 1) else a
    go(0, 1, n)
  }

  println("fibonnaci" + fib(2))

  def check[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def check1[A](n: Int) = {
      if (as.isEmpty) true
      else if (ordered(as(n), as(n + 1))) false
      else ordered(n + 1)
    }
    check1(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { a: A => ((b: B) =>
    f(a, b))
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { //can be written as A=>(B=>C) as associates towards right
    (a: A, b: B) =>
      f(a)(b) //how it works -> what is output of function(a) i.e b it is passed to again f(output of function(a) i.e b ) leads to c
    /*you actually
        get two traditional function invocations back to back*/
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = { (a: A) =>
    f(g(a))
  }

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil          => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs)  => x * product(xs)
    }

    /*as argument changes from type A to seq[A] variable arguments n extractors refer to martin's book for deatailed explanation
object List {
def apply[T](elems: T*) = elems.toList
def unapplySeq[T](x: List[T]): Option[Seq[T]] = Some(x)
...
}*/

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}
