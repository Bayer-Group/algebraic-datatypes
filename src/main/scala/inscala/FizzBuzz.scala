package inscala

object FizzBuzzStraight extends App {

  def printIt(i: Int): Unit =
    if (i % 3 == 0 && i % 5 == 0)
      println("FizzBuzz")
    else if (i % 3 == 0)
      println("Fizz")
    else if (i % 5 == 0)
      println("Buzz")
    else
      println(String.valueOf(i))

  for (i <- 1 to 100) {
    printIt(i)
  }
}

object FizzBuzzIdiomatic extends App {

  val isMod: (Int, Int) => Boolean = (m, i) => i % m == 0
  val mod3 = isMod(3, _)
  val mod5 = isMod(5, _)

  val fizzBuzzIt: Int => String = {
    case i if mod3(i) && mod5(i) => "FizzBuzz"
    case i if mod3(i)            => "Fizz"
    case i if mod5(i)            => "Buzz"
    case i                       => i.toString
  }

  Stream(1 to 100: _*).map(fizzBuzzIt).foreach(println)

  Stream(1 to 100: _*).map(fizzBuzzIt).filter(_ != "Fizz").foreach(println)
}
