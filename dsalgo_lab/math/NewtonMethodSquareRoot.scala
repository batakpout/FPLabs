package dsalgo_lab.math

object NewtonSquareRoot extends App {
   /**
   * The classical way to achieve this is by successive approximations using Newton’s method
   * x(n) = x - f(x)/f(x')
   * e.g x = squareroot(N) -> x * x - n = 0
   * x(n) = x-(x*x-n) / 2x ==> 2x^2 - x^2 + N / 2x => x^2 + n / 2x => x + n/x / 2
   * x + n/x ==> nextGuess = (guess + N / guess) / 2
   */
  def abs(x: Double): Double = if (x < 0) -x else x

  def improve(guess: Double, x: Double): Double = (guess + x / guess) / 2

  def isGoodEnough(guess: Double, x: Double) = abs(guess * guess - x) < 0.001

  //def isGoodEnough(guess: Double, x: Double) = abs(guess * guess - x) / 2 < 0.001 -- fit for large +ive & -ive numbers
  def sqrItr(guess: Double, x: Double): Double = {
    if (isGoodEnough(guess, x)) guess else sqrItr(improve(guess, x), x)
  }

  def squareRoot(n: Int) = sqrItr(1.0, n)

  def isPerfectSquare(number: Int): Boolean = {
    val sqrtValue = squareRoot(number) //Math.sqrt(number)
    val truncatedSqrt = sqrtValue.toInt
    truncatedSqrt * truncatedSqrt == number
  }

  val result = (1 to 20).toList.filter(isPerfectSquare)
  println(result)
}
