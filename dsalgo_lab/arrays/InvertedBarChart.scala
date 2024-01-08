package dsalgo_lab.arrays

object InvertedBarChart extends App {

  def solution(a: Array[Int]): Unit = {
    def findMax(index: Int = 1, max: Int = a(0)): Int = {
      if (index < a.length) {
        if (max > a(index)) findMax(index + 1, max)
        else findMax(index + 1, a(index))
      } else max
    }

    val max = findMax()
    var i = 1
    while(i > 0 && i <= max) {
      a.foreach { item =>
         if(item >= i) print("*\t")
         else print("\t")
      }
      println()
      i += 1
    }
  }

  solution(Array(3,1,0,5,7))
}
