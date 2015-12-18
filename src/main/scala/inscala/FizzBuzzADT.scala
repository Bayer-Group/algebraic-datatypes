package inscala

sealed abstract class FizzBuzzADT(i: Int) {
  override def toString: String = i.toString
}

object FizzBuzzADT {
  def apply(i: Int): FizzBuzzADT = i match {
    case _ if i % 3 == 0 && i % 5 == 0 => FizzBuzz(i)
    case _ if i % 3 == 0               => Fizz(i)
    case _ if i % 5 == 0               => Buzz(i)
    case _                             => JustInt(i)
  }
}

case class Fizz(i: Int) extends FizzBuzzADT(i) {
  override val toString = "Fizz"
}

case class Buzz(i: Int) extends FizzBuzzADT(i) {
  override val toString = "Buzz"
}

case class FizzBuzz(i: Int) extends FizzBuzzADT(i) {
  override val toString = "FizzBuzz"
}

case class JustInt(i: Int) extends FizzBuzzADT(i)

object FizzBuzzWithADT extends App {

  // Just run the algorithm
  Stream(1 to 100: _*).map(FizzBuzzADT(_)).foreach(println)


  // Filter out the Fizz using pattern matching
  Stream(1 to 100: _*).map(FizzBuzzADT(_)).foreach {
    case Fizz(_) => // I don't like Fizz
    case x    => println(x)
  }


  // Filter out the Fizz using the type
  Stream(1 to 100: _*).map(FizzBuzzADT(_)).filter(!_.isInstanceOf[Fizz]).foreach(println)


  // Filter out the odd numbers after the fact
  val even: Int => Boolean = i => i % 2 == 0

  // Pretend the Stream[Int] came from somewhere else
  Stream(1 to 100: _*).map(FizzBuzzADT(_)).foreach {
    case a@Fizz(i) if even(i)     => println(a)
    case a@Buzz(i) if even(i)     => println(a)
    case a@FizzBuzz(i) if even(i) => println(a)
    case a@JustInt(i) if even(i)  => println(a)
    case _                          => // Be quiet
  }
}
