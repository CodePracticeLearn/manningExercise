object UnitTests {

  def main(args:Array[String]) ={
    val check1 = test.foldRight[Int,Int](test.List(1,2,3),0)(_ + _)

    println("the printed stucture looks like this"+check1)
  }

}
