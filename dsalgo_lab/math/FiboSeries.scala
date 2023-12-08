package dsalgo_lab.math

object Fibo extends App {

  def fiboSeries(till: Int): List[Int] = {
    def fibo(c: Int, n: Int, list: List[Int]): List[Int] = {
      if (c > till) list.reverse else {
        fibo(n, c + n, c :: list)
      }
    }
    fibo(0, 1, Nil)
  }
  println(fiboSeries(20))
}
