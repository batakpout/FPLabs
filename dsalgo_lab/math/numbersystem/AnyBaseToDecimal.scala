package dsalgo_lab.math.numbersystem

object AnyBaseToDecimal extends App {

  def f(n: Int, currentBase: Int): Int = {
    def calculate(n: Int, raiseToPower: Int, result: Int = 0): Int = {
      if (n == 0) result else {
        calculate(n / 10, raiseToPower + 1, result + (n % 10  * Math.pow(currentBase, raiseToPower).toInt))
      }
    }

    calculate(n, 0)
  }

  print(f(76543, 8))
}
