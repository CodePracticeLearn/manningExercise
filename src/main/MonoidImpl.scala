

object MonoidImpl {

  /* Just what is a monoid? It’s simply a type A and an implementation of Monoid[A] that satisfies the laws.
  Stated tersely, a monoid is a type together with a binary operation (op) over that type, satisfying associativity
  and having an identity element (zero).*/

  trait Monoid[A] {
    def operation(a1: A, a2: A): A

    def zero: A
  }

  //Give Monoid instances for integer addition and multiplication as well as the Boolean operators.
  val intAddition: Monoid[Int] = {
    def operation(x: Int, y: Int): Int = x + y

    def fun: Int = 0 // think like identity functions
  }

  val intMultiplication = new Monoid[Int] = {
    def operation(x: Int, y: Int): Int = x * y

    def fun: Int = 1 // think like identity functions
  }

  val booleanOr = new Monoid[Boolean] = {
    def operation(x: Boolean, y: Boolean): Boolean = x || y

    def fun: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] = {
    def operation(x: Boolean, y: Boolean): Boolean = x && y

    def fun: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = {
     def operation(a:Option[A],b:Option[A]):Option[A] = a orElse b
     def zero: Option[A] = None  }


}
