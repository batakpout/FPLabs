package dsalgo_lab.math.numbersystem

object AnyBaseMultiplication extends App {


  def multiply(num1: Int, num2: Int, base: Int): Int = {
    if (num1 < num2) multiply(num2, num1, base) else {
      def calculate(n1: Int, n2: Int, carry: Int = 0, mul: Int = 1, result: Int = 0): Int = {
        if (n1 == 0) {
          if(carry == 0) result
          else result + (carry * mul)
        } else {
          val d = n1 % 10
          val m = (d * n2) + carry
          calculate(n1 / 10, n2, m / base, mul * 10, result + (mul * (m % base)))
        }
      }

      def f(n: Int, mul: Int = 1, result: Int = 0): Int = {
        if (n == 0) result else {
          val sum = calculate(num1, n % 10)
          f(n / 10, mul * 10, AnyBaseAddition.add(result, sum * mul, base))
        }
      }

      f(num2)
    }
  }

  val result = multiply(1111, 10101, 2)
  println(result)
}
