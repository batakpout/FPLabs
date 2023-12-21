package dsalgo_lab.math.numbersystem

object AnyBaseSubstraction extends App {

  def substract(num1: Int, num2: Int, base: Int): Int = {

    if (num1 < num2) substract(num2, num1, base) else {
      def calculate(n1: Int, n2: Int, mul: Int = 1, carry: Int = 0, result: Int = 0): Int = {

        if (n1 == 0 && n2 == 0) {
          result
        } else {
          val d1 = (n1 % 10) + carry
          val d2 = n2 % 10
          if (d1 < d2) {
            val sub = (d1 + base) - d2
            calculate(n1 / 10, n2 / 10, mul * 10, -1, result + (mul * sub))
          } else
            calculate(n1 / 10, n2 / 10, mul * 10, 0, result + (mul * (d1 - d2)))
        }
      }

      calculate(num1, num2)
    }
  }

  val res = substract(1, 100, 8)
  println(res)
}
