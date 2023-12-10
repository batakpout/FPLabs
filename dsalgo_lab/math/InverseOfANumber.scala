package dsalgo_lab.math

/*
index 6 5 4 3 2 1
 426135
 e.g if at index 1 there is 5, then in result at 5th index we will have 1
5 4 3 2 1
416253
 */
object InverseOfANumber extends App {

  val div = 10

  def inverse(num: Int, pos: Int = 1, result: Int = 0): Int = {
    if (num == 0) result
    else {
      val d = num / div
      val r = num % div
      inverse(d, pos + 1, result + (pos * Math.pow(10, r - 1).toInt))
    }
  }

  println(inverse(132))
  println(inverse(426135))
  println(inverse(43125))
}
