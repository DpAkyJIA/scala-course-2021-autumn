package karazin.scala.users.group.week1.homework

import org.scalacheck._
import Prop.{forAll, propBoolean}
import Homework._
import karazin.scala.users.group.week1.homework.arbitraries

object HomeworkSpecification extends Properties("Homework"):

  include(BooleanOperatorsSpecification)
  include(FermatNumbersSpecification)
  include(LookAndAaSequenceSpecification)

end HomeworkSpecification

object BooleanOperatorsSpecification extends Properties("Boolean Operators"):
  import `Boolean Operators`._

  property("not") = forAll { (b: Boolean) =>
    not(b) == !b
  }

  property("and") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    and(left, right) == (left && right)
  }

  property("or") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    or(left, right) == left || right
  }

end BooleanOperatorsSpecification

object FermatNumbersSpecification extends Properties("Fermat Numbers"):
  import `Fermat Numbers`._
  import arbitraries.given Arbitrary[Int]

  property("multiplication") = forAll { (left: Int, right: Int) =>
    multiplication(left, right) == (left * right)
  }

  property("power") = forAll { (left: Int, right: Int) =>
    power(left, right) == (0 until right).foldLeft(BigInt(1)) { (acc, _) => acc * left }
  }

  property("fermatNumber") = forAll { (n: Int) =>
    if n >= 0 then fermatNumber(n).toInt == (Math.pow(2, Math.pow(2, n)) + 1)
    else true
  }

end FermatNumbersSpecification

object LookAndAaSequenceSpecification extends Properties("Look-and-say Sequence"):
  import `Look-and-say Sequence`._
  import arbitraries.given Arbitrary[Int]

  property("fermatNumber") = forAll { (n: Int) =>
    def countnndSay(n: Int): String = { // Base cases
      if (n == 1) return "1"
      if (n == 2) return "11"
      // Find n'th term by generating
      // all terms from 3 to n-1.
      // Every term is generated
      // using previous term
      // Initialize previous term
      var str = "11"
      for (i <- 3 to n) { // In below for loop, previous
        // character is processed in
        // current iteration. That is
        // why a dummy character is
        // added to make sure that loop
        // runs one extra iteration.
        str += '$'
        val len = str.length
        var cnt = 1 // Initialize count
        // of matching chars
        var tmp = "" // Initialize i'th
        // term in series
        val arr = str.toCharArray
        // Process previous term
        // to find the next term
        for (j <- 1 until len) { // If current character
          // does't match
          if (arr(j) != arr(j - 1)) { // Append count of
            // str[j-1] to temp
            tmp += cnt + 0
            // Append str[j-1]
            tmp += arr(j - 1)
            // Reset count
            cnt = 1
          }
          else { // If matches, then increment
            // count of matching characters
            cnt += 1
          }
        }
        // Update str
        str = tmp
      }
      str
    }

    lookAndSaySequenceElement(n) == BigInt(countnndSay(n))
  }

end LookAndAaSequenceSpecification