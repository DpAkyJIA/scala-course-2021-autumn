package karazin.scala.users.group.week3

import scala.annotation.tailrec

object Homework:

  // Peano numbers
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ(this)

    infix def + (that: Nat): Nat

    infix def - (that: Nat): Nat

    // Optional task
    def toInt: Int

    // Optional task
    def fromInt(int: Int): Nat =
      @tailrec
      def fromIntRec(int: Int, nat: Nat): Nat =
        if int == 0 then nat
        else fromIntRec(int - 1, nat.successor)
      fromIntRec(int, Zero)

    override def toString: String = s"Nat($predecessor)"

  type Zero = Zero.type
  object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")

    infix def +(that: Nat): Nat = that

    infix def -(that: Nat): Nat = throw new Exception("A Nat can't be negative")

    // Optional task
    def toInt: Int = 0

    override def toString: String = "Zero"
    override def equals(obj: Any): Boolean = obj match {
      case that: Succ =>
        (that canEqual this) &&
           that.isZero
      case _ => false
    }

  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = n

    infix def +(that: Nat): Nat =
      if that.isZero then this
      else n + that.successor

    infix def -(that: Nat): Nat =
      if that.isZero then this
      else n - that.predecessor

    // Optional task
    def toInt: Int =
      @tailrec
      def toIntRec(number: Nat, i: Int): Int =
        if number.isZero then i
        else toIntRec(number.predecessor, i + 1)
      toIntRec(this, 0)


    def canEqual(a: Any): Boolean = a.isInstanceOf[Succ]

    override def equals(obj: Any): Boolean = obj match {
      case that: Succ =>
        (that canEqual this) &&
          successor == that.successor
      case _ => false
    }

  def main(args: Array[String]): Unit = {
/*    val number0 = Zero
    val number = Succ(Zero)

    val num0 = number.toInt

    val tmp = number0.fromInt(1)

    println(tmp)*/
  }

end Homework

