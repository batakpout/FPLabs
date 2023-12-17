package dsalgo_lab.math

// 4 occurs 3 times in 994543234

object DigitFrequency extends App {

  def digitFreq(n: Int, d: Int): Int = {
    def freq(n: Int, count: Int = 0): Int = {
      if (n == 0) count else {
        if (n % 10 == d) freq(n / 10, count + 1)
        else freq(n / 10, count)
      }
    }
    freq(n)
  }
 val result =  digitFreq(994543234, 4)
  println(result)
}
