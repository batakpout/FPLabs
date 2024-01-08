package dsalgo_lab.leetcode

//Difficulty: Easy
//Problem: Reverse String II
//TC: O(N)

object Problem541FP extends App {


  val k = 2
  val str = "abcd"

  val result = str.grouped(k * 2).map(s => s.take(k).reverse ++ s.drop(k)).toList.mkString
  println(result)
}

object Problem541FasterVersion extends App {

  def rev(s: String, l: Int, e: Int): String = {
    def r(c: Array[Char], l1: Int, e1: Int): Array[Char] = {
      if (l1 < e1) {
        val temp = c(l1)
        c(l1) = c(e1)
        c(e1) = temp
        r(c, l1 + 1, e1 - 1)
      } else c
    }
    r(s.toCharArray, l, e).mkString
  }

  def solution(s: String, k: Int): String = {
    val l = 0
    val h = Integer.min(s.length - 1, l + (k - 1))

    def r(s: String, l: Int, h: Int): String = {
      if (l < s.length) {
        val x = l + 2 * k
        r(rev(s, l, h), x, Integer.min(s.length - 1, x + (k - 1)))
      } else s
    }

    r(s, l, h)
  }

  println(solution("abcdefjhijklmn", 3))

}
