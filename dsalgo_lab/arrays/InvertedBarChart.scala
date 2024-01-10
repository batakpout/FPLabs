package dsalgo_lab.arrays

object InvertedBarChart extends App {

  def solution(a: Array[Int]): Unit = {
    def findMax(index: Int = 1, max: Int = a(0)): Int = {
      if (index < a.length) {
        if (max > a(index)) findMax(index + 1, max)
        else findMax(index + 1, a(index))
      } else max
    }

    def printStars(i: Int, max: Int): Unit = {
      if (i > 0 && i <= max) {
        a.foreach { item =>
          if (item >= i) print("*\t")
          else print("\t")
        }
        println()
        printStars(i + 1, max)
      }
    }

    val max = findMax()
    printStars(1, max)
  }

  solution(Array(3,1,0,5,7))
}
