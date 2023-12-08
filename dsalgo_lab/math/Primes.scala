package dsalgo_lab.math

object Primes extends App {

  def isPrime(n: Int) = {

    def checkRec(div: Int): String = {
      if (div * div <= n)
        if (n % div == 0) "not prime"
        else checkRec(div + 1)
      else "prime"
    }
    checkRec(2)
  }

  def isPrimeVersion2(n: Int) = {
    if (n <= 1) false else {
      val sqrt = Math.sqrt(n).toInt
      !(2 to sqrt).exists(n % _ == 0)
    }
  }

  println(isPrime(3))
  println(isPrime(13))
  println(isPrime(37))
  println(isPrime(100))

  def primeSeries(low: Int, high: Int) = {
    (low to high).filter(isPrimeVersion2)
  }

  println(primeSeries(2, 20))

}
