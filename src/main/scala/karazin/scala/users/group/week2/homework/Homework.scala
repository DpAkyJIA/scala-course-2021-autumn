package karazin.scala.users.group.week2.homework

import scala.annotation.{tailrec, targetName}
import scala.math.{abs, signum}

object Homework:

  // `x` and `y` are inaccessible from outside
  class Rational(x: Int, y: Int):
    // Checking the precondition. Is fails then throws `IllegalArgumentException`
    require(y > 0, "Denominator must be positive")

    def this(x: Int) = this(x, 1)

    val numerator: Int = x / g
    val denominator: Int = y / g

    // Defines an external name for a definition
    @targetName("less than")
    // Annotation on a method definition allows using the method as an infix operation
    infix def <(that: Rational): Boolean =
      this.numerator * that.denominator < that.numerator * this.denominator

    @targetName("less or equal")
    infix def <=(that: Rational): Boolean =
      this < that || this == that

    @targetName("greater than")
    infix def >(that: Rational): Boolean =
      !(this <= that)

    @targetName("greater or equal")
    infix def >=(that: Rational): Boolean =
      !(this < that)

    @targetName("addition")
    infix def +(that: Rational): Rational =
      new Rational(this.numerator * that.denominator + that.numerator * this.denominator, this.denominator * that.denominator)

    @targetName("negation")
    infix def unary_- : Rational =
      new Rational(numerator * -1, denominator)

    @targetName("subtraction")
    infix def -(that: Rational): Rational =
      new Rational(this.numerator * that.denominator - that.numerator * this.denominator, this.denominator * that.denominator)

    @targetName("multiplication")
    infix def *(that: Rational): Rational =
      new Rational(this.numerator * that.numerator, this.denominator * that.denominator)

    @targetName("division")
    infix def /(that: Rational): Rational =
      new Rational(this.numerator * that.denominator, this.denominator * that.numerator)

    override def toString: String = s"${this.numerator}/${this.denominator}"

    @tailrec
    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    private lazy val g = gcd(abs(x), y)

    def canEqual(a: Any): Boolean = a.isInstanceOf[Rational]

    override def hashCode(): Int = {
      val state = Seq(numerator, denominator, g)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }

    override def equals(other: Any): Boolean = other match {
      case that: Rational =>
        (that canEqual this) &&
          numerator == that.numerator &&
          denominator == that.denominator &&
          g == that.g
      case _ => false
    }
  end Rational

  def main (args: Array[String]): Unit = {
/*    val number1 = new Rational(1, 2)
    val number2 = new Rational(2, 3)

    println(number1.toString())
    println(number2.toString())

    println((number1 + number2).toString())
    println((number1 - number2).toString())
    println((number1 * number2).toString())
    println((number1 / number2).toString())

    println(number1.unary_-.toString())

    println(number1.equals(number2))*/
  }

end Homework