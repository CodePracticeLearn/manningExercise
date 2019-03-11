import scala.annotation.tailrec

object Implementation{

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
// easy implementation


  /*def foldLeft1[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def iter(as: List[A], acc: B): B = as match {
      case Nil => acc
      case h :: t => iter(t, f(acc, h))
    }
    iter(as, z)
  }*/

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

  def append[A](xs: List[A], ys: List[A]): List[A] = foldRight(xs,ys)(Cons(_ , _))


 // def flatten[A](xs: List[List[A]]): List[A] = foldRight(Nil:List[A],xs)(append)

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))


  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)


  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }



  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree{


    def size[A](t:Tree[A]):Int ={
      case Leaf(_) => 1
      case Branch(l,r) => 1 + size(l) + size(r)
    }

    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(n) => n
      case Branch(l,r) => maximum(l) max maximum(r)
    }

    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 0
      case Branch(l,r) => 1 + (depth(l) max depth(r))
    }

    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }

    def foldT[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
      case Leaf(a) => f(a)
      case Branch(left, right) => g(foldT(left)(f)(g), foldT(right)(f)(g))
    }

    def fSize(tree: Tree[_]): Int = foldT(tree)(_ => 1)((left, right) => left + right + 1)
    def fMax(tree: Tree[Int]): Int = foldT(tree)(identity)(_ max _)
    def fDepth(tree: Tree[_]): Int = foldT(tree)(_ => 1)((x, y) => (x max y) + 1)
    def fMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = foldT(tree)(x => Leaf(f(x)): Tree[B])((x, y) => Branch(x, y))
  }
}
