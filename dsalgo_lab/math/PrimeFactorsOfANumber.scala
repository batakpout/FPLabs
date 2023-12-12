package dsalgo_lab.math

object PrimeFactorsOfANumber extends App {


  def primeFactors(n: Int): List[Int] = {

    def find(num: Int, div: Int = 2, list: List[Int] = Nil): List[Int] = {
      if (div * div <= n) {
        if (num % div == 0) find(num / div, div, div :: list)
        else find(num, div + 1, list)
      } else {
        if (num != 1) List(num) else list.reverse
      }
    }
    find(n)
  }

  println(primeFactors(24))
  println(primeFactors(1440))
  println(primeFactors(37))
  println(primeFactors(23))
}
