

object MonoidImpl {

  //Give Monoid instances for integer addition and multiplication as well as the Boolean operators.
  val intAddition: Monoid[Int] = {
    def add(x: Int, y: Int): Int = x + y

    def fun: Int = 0 // think like identity functions
  }

  val intMultiplication = new Monoid[Int] {
    def mul(x: Int, y: Int): Int = x * y

    def fun: Int = 1 // think like identity functions
  }

  val booleanOr = new Monoid[Boolean] {
    def bool(x: Boolean, y: Boolean): Boolean = x || y

    def fun: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def boolA(x: Boolean, y: Boolean): Boolean = x && y

    def fun: Boolean = true
  }
}
