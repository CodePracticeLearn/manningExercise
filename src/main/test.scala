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

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {}

}
