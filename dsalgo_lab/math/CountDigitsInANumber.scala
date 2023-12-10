package dsalgo_lab.math

object CountDigitsInANumber extends App {

  def count(n: Int, c: Int = 0): Int = n / 10 match {
    case i if i == 0 => c + 1
    case _ => count(n / 10, c + 1)
  }

  println(count(65784383))
}
