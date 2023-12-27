package dsalgo_lab.leetcode

//Difficulty: Easy
//Problem: Convert Roman to Integer
//TC: O(N), space: O(128) = O(1)
object Problem13Naive extends App {


  val roman = "MCMXCIV"

  def check(s: String): Int = {
    val a = new Array[Int](128)
    a('I') = 1
    a('V') = 5
    a('X') = 10
    a('L') = 50
    a('C') = 100
    a('D') = 500
    a('M') = 1000

    def convert(i: Int = 0, r: Int = 0): Int = {
      if (i + 1 < s.length) {
        if (a(s.charAt(i)) >= a(s.charAt(i + 1)))
          convert(i + 1, r + a(s.charAt(i)))
        else
          convert(i + 1, r - a(s.charAt(i)))
      } else r + a(s.charAt(i))
    }

    convert()
  }

  println(check(roman))

}

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.scalacheck.Checkers

class CustomStringGenSpec extends AnyFunSuiteLike with Checkers {
  val validChars: List[Char] = List('I', 'V', 'X', 'L', 'C', 'D', 'M')

  test("Generated string should have length less than 15 and contain valid characters") {
    check {
      forAll(customStringGen) { str =>
        println(str)
        val res = Problem13Naive.check(str)
        println(res)
        str.length <= 14 && str.forall(validChars.contains)
      }
    }
  }

  def customStringGen: Gen[String] = {
    for {
      length <- Gen.choose(3, 14)
      chars <- Gen.listOfN(length, Gen.oneOf(validChars))
    } yield chars.mkString
  }
}
