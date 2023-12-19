package dsalgo_lab.math.numbersystem

object AnyBaseToAnyBase extends App {


  def f(n: Int, currentBase: Int, toBase: Int): Int = {
    val decimal = AnyBaseToDecimal.f(n, currentBase)
    DecimalToAnyBase.f(decimal, toBase)
  }

  println(f(110111, 2, 8))
}
