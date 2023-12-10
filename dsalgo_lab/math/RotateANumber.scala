package dsalgo_lab.math

//rotate right
object RotateANumberFP extends App {

  val l = List(2, 7, 3, 9, 1)
  val length = l.length
  val k = 7
  val split = if (k > length) k - length else length - k
  println(split)
  val splitList: (List[Int], List[Int]) = l.splitAt(split)
  println(splitList._2 ++ splitList._1)
}

object OtherApproach extends App {

  def rotate(num: Int, k: Int): Int = {

    def length(num: Int, count: Int = 0): Int = {
      if (num == 0) count else length(num / 10, count + 1)
    }

    def getDivMul(length: Int, w: Int, mul: Int = 1, div: Int = 1, count: Int = 1): (Int, Int) = {
      if (length == 0) (mul, div) else {
        if (count <= w) getDivMul(length - 1, w, mul, div * 10, count + 1)
        else getDivMul(length - 1, w, mul * 10, div, count + 1)
      }
    }

    val l = length(num)
    val window = k % l
    val (mul, div) = getDivMul(l, if (window < 0) window + l else window)
    ((num % div) * mul) + num / div
  }

  println(27391)
  println("----")
  val result1 = rotate(27391, 1)
  val result2 = rotate(27391, 2)
  val result3 = rotate(27391, 3)
  val result4 = rotate(27391, 4)
  val result5 = rotate(27391, 5)

  val result6 = rotate(27391, 6)
  val result7 = rotate(27391, 7)
  val result8 = rotate(27391, -1)

  println(result1)
  println(result2)
  println(result3)
  println(result4)
  println(result5)
  println(result6)
  println(result7)
  println(result8)
}
