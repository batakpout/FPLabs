package dsalgo_lab.math.numbersystem

object AnyBaseAddition extends App {


  def add(num1: Int, num2: Int, base: Int) = {

    def calculate(n1: Int, n2: Int, mul: Int = 1, carry: Int = 0, result: Int = 0): Int = {
      if (n1 == 0 && n2 == 0) {
        if (carry == 0) result
        else result + (mul * carry)
      } else {
        val d1 = n1 % 10
        val d2 = n2 % 10
        val sum = carry + d1 + d2
        calculate(n1 / 10, n2 / 10, mul * 10, sum / base, result + (mul * (sum % base)))
      }
    }
    calculate(num1, num2)
  }

  val result1 = add(234, 343, 5)
  val result2 = add(346, 777, 8)
  val result3 = add(112, 222, 10)
  val result4 = add(101, 101, 2)
  val result5 = add(2, 2319, 10)
  val result6 = add(44, 44344, 5)
  val result7 = add(777, 1, 8)

   assert(result1 == 1132)
   assert(result2 == 1345)
   assert(result3 == 334)
   assert(result4 == 1010)
   assert(result5 == 2321)
   assert(result6 == 44443)
   assert(result7 == 1000)

}
