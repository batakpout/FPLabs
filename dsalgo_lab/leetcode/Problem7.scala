package dsalgo_lab.leetcode


object ReverseANumber extends App {

  def reverse(x: Long): Long = {

    val div = 10

    def rev(num: Long, result: Long): Long = {
      if (num == 0) result else {
        val q = num / div
        val rem = num % div
        rev(q, rem + (result * div))
      }
    }

    val result = rev(if (x < 0) -x else x, 0)
    (if (result > Integer.MAX_VALUE || result < Integer.MIN_VALUE) 0 else if (x < 0) -result else result).toInt

  }

  println(reverse(234))
  println("--------")

  println(reverse(2147483647))
  println("--------")

  println(reverse(-2147483647))
  println("--------")

  println(reverse(120))
  println("--------")

  println(reverse(0))
}
