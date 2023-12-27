package dsalgo_lab.leetcode

//Difficulty: Easy
//Problem: Convert Roman to Integer

object Problem13Naive extends App {


  val roman = "MCMXCIV"

  def check(s: String): Int = {
    val a = new Array[Int](238)
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
