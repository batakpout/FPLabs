package dsalgo_lab.math.numbersystem

object DecimalToAnyBase extends App {

  def f(n: Int, changeToBase: Int): Int = {
    def calculate(n: Int, currentBase: Int, result: Int = 0): Int = {
      if (n == 0) result else {
        calculate(n / changeToBase, currentBase * 10, result + (currentBase  * (n % changeToBase)))
      }
    }

    calculate(n, 1)
  }

  print(f(212, 8))
}
