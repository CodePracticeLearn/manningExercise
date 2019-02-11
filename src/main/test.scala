import scala.annotation.tailrec

object test{

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, n: Int): Int = if (n > 0) go(b, a + b, n - 1) else a
    go(0, 1, n)
  }

  def check[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def check1[A](n: Int) = {
      if (as.isEmpty) true
      else if (ordered(as(n), as(n + 1))) false
      else true
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

  sealed trait List[+A] // Adding sealed in front means that all implementations of the trait must be declared in this file

  case object Nil extends List[Nothing] //cannot take paramters

  case class Cons[+A](head: A, tail: List[A]) extends List[A] //can take params as case class not object

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

  def tails[A](ls: List[A]): List[A] = {

    ls match {
      case Nil        => List()
      case Cons(_, t) => t
    }
  }
  def setHead[A](ls: List[A], n: A): List[A] = {
    ls match {
      case Nil        => List()
      case Cons(x, t) => Cons(x, t)
    }

  }

  def drop[A](ls: List[A], n: Int): List[A] = {
    ls match {

      case Nil        => List()
      case Cons(x, t) => drop(t, n - 1)
    }

  }

  def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
    case Cons(x, t) if f(x) => dropWhile(t, f)
    case _                  => ls
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => List()// any be error or exception too
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](ls: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    ls match {
      case Nil => z
      case Cons(head, xs) => f(head, foldRight(xs, z)(f))
    }

  def length[A](ls: List[A]): Int =
    foldRight(ls, 0)((_,acc) => acc + 1)


  @annotation.tailrec
  def foldLeft[A,B](ls: List[A], z: B)(f: (B, A) => B): B = ls match {
    case Nil => z
    case Cons(head,tail) => foldLeft(tail, f(z,head))(f)
  }

  /*
  * foldLeft((1,Cons(2,Cons(3,Nil)),0)(_+_)
  * foldleft(cons(2,cons(3,Nil)),1)
  */

// do read this answer chain for better understanding: https://stackoverflow.com/questions/17136794/foldleft-using-foldright-in-scala
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    //foldLeft(reverse(l), z)((b,a) => f(a,b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
  // foldright is not tail recursive, so foldRightViaFoldLeft_1 is used to avoid stack overflow





}
