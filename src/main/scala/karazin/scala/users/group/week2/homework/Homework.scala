package karazin.scala.users.group.week2.homework

import scala.annotation.{tailrec, targetName}
import scala.math.{abs, signum}

object Homework:

  // `x` and `y` are inaccessible from outside
  class Rational(x: Int, y: Int):
    // Checking the precondition. Is fails then throws `IllegalArgumentException`
    require(y > 0, "Denominator must be positive")

    def this(x: Int) = this(x, 1)

    val numer: Int = x / g
    val denom: Int = y / g

    // Defines an external name for a definition
    @targetName("less than")
    // Annotation on a method definition allows using the method as an infix operation
    infix def <(that: Rational): Boolean =
      this.numer * that.denom < that.numer * this.denom

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
      new Rational(this.numer * that.denom + that.numer * this.denom, this.denom * that.denom)

    @targetName("negation")
    infix def unary_- : Rational =
      new Rational(numer * -1, denom)

    @targetName("subtraction")
    infix def -(that: Rational): Rational =
      new Rational(this.numer * that.denom - that.numer * this.denom, this.denom * that.denom)

    @targetName("multiplication")
    infix def *(that: Rational): Rational =
      new Rational(this.numer * that.numer, this.denom * that.denom)

    @targetName("division")
    infix def /(that: Rational): Rational =
      if that.numer == 0 then throw new Exception("You cannot divide by zero")
      else if that.numer > 0 then new Rational(this.numer * that.denom, this.denom * that.numer)
      else new Rational(this.numer * that.denom * -1, this.denom * that.numer * -1)

    override def toString: String = s"${this.numer}/${this.denom}"

    @tailrec
    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    private lazy val g = gcd(abs(x), y)

    def canEqual(a: Any): Boolean = a.isInstanceOf[Rational]

    override def hashCode(): Int = {
      val state = Seq(numer, denom, g)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }

    override def equals(other: Any): Boolean = other match {
      case that: Rational =>
        (that canEqual this) &&
          numer == that.numer &&
          denom == that.denom &&
          g == that.g
      case _ => false
    }
  end Rational

end Homework