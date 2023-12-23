package dsalgo_lab.arrays

object SumOfTwoArrays extends App {

  def sum(a1: Array[Int], a2: Array[Int]): Unit = {

    val n1 = a1.length
    val n2 = a2.length
    val sum = if (n1 > n2) new Array[Int](n1) else new Array[Int](n2)

    def calculate(i: Int, j: Int, k: Int, c: Int = 0): Int = {
      if (k < 0) c
      else {
        val d = c + (if (i >= 0) a1(i) else 0) + (if (j >= 0) a2(j) else 0)
        sum(k) = d % 10
        calculate(i - 1, j - 1, k - 1, d / 10)
      }
    }

    val carry = calculate(n1 - 1, n2 - 1, sum.length - 1)
    if (carry != 0) print(carry + "\t")
    sum.foreach(x => print(x + "\t"))
  }

  sum(Array(9, 1, 7, 4, 5), Array(4, 7, 8, 1, 2, 4, 6, 8))
}

object SumUsingList extends App {
  def sum(l1: List[Int], l2: List[Int]): List[Int] = {
    l1.zip(l2).map { case (a, b) => a + b}
  }

  println(sum(List(12,2,3), List(4,22,21)))
}
