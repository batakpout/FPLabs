package dsalgo_lab.math

object CountDigitsInANumber extends App {

  def count(n: Int, c: Int = 0): Int =
    if(n == 0) c else count(n / 10, c + 1)


  println(count(65784383))
}
