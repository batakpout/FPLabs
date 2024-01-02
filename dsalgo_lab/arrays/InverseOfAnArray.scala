package dsalgo_lab.arrays

object InverseOfAnArray extends App {


  def inverse(a: Array[Int]): Array[Int] = {
    val res = new Array[Int](a.length)

    def rec(index: Int): Array[Int] = {
      if (index < a.length) {
        res(a(index)) = index
        rec(index + 1)
      } else res
    }

    rec(0)

  }

  val arr = Array(3, 2, 4, 1, 0)
  println {
    inverse(arr).mkString(",")
  }

}
