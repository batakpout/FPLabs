package dsalgo_lab.leetcode

//Difficulty: Easy
//Problem: check if a number is palindrome without converting it to string
/**
  Time: O(log10(N))
  Space: O(1)
 */
object Problem9 extends App {

  def isPalindrome(x: Int): Boolean = x match {
    case 0 => true
    case i if i < 0 => false
    case i =>

      def reverse(i: Int, r: Int = 0): Int = {
        if (i == 0) r else {
          reverse(i / 10, i % 10 + (r * 10))
        }
      }

      i == reverse(i)

  }

  println(isPalindrome(0))
  println(isPalindrome(-232))
  println(isPalindrome(-1212))
  println(isPalindrome(121))
  println(isPalindrome(34143))

}
