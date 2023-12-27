package dsalgo_lab.leetcode

//Difficulty: Easy
//Problem: reverse a char array
//must modify input array in-place with O(1) extra memory
//TC: O(N), Space:O(1)
object Problem344 extends App {


  def reverse(a: Array[Char]): Array[Char] = {
    def rec(s: Int, e: Int): Array[Char] = {
      if (s < e) {
        val temp = a(s)
        a(s) = a(e)
        a(e) = temp
        rec(s + 1, e - 1)
      } else a
    }

    rec(0, a.length - 1)
  }

  def reverse2(a: Array[Char]): Array[Char] = {

    var s = 0
    var e = a.length - 1
    while (s < e) {
      val temp = a(s)
      a(s) = a(e)
      a(e) = temp
      s += 1
      e -=1
    }
    a
  }

  println(reverse2(Array('h', 'a', 'n', 'n', 'a', 'h')).mkString("[", ",", "]"))
}
