package dsalgo_lab.arrays

/**
 * arr = (15, 30, 40, 4, 11, 9); max = 40, min = 4, so span = max-min = 36
 */
object NaiveSpanOfAnArray extends App {
  //find max and min of an array
  // TC: O(N) + O(N) = O(2N)
  def spanOfAnArray(arr: Array[Int]): Int = {

    def findMax(arr: Array[Int], max: Int): Int = {
      def calculate(index: Int, currentMax: Int): Int = {
        if (index < arr.length) {
          calculate(index + 1, if (arr(index) > currentMax) arr(index) else currentMax)
        } else currentMax
      }

      calculate(0, max)
    }

    def findMin(arr: Array[Int], min: Int): Int = {
      def calculate(index: Int, currentMin: Int): Int = {
        if (index < arr.length) {
          calculate(index + 1, if (arr(index) < currentMin) arr(index) else currentMin)
        } else currentMin
      }

      calculate(0, min)
    }

    val max = findMax(arr.tail, arr.head)
    val min = findMin(arr.tail, arr.head)

    max - min

  }

  val result = spanOfAnArray(Array(15, 30, 40, 4, 11, 9))
  println(result)


}

object BetterSolutionSpanOfAnArray extends App {
  //TC = O(N)

  def span(arr: Array[Int]): Int = {

    def find(arr: Array[Int], max: Int, min: Int, index: Int = 1): Int = {
      if (index < arr.length) {
        val updatedMax = if (arr(index) > max) arr(index) else max
        val updatedMin = if (arr(index) < min) arr(index) else min
        find(arr, updatedMax, updatedMin, index + 1)
      } else max - min
    }

    find(arr, arr(0), arr(0))
  }

  println(span(Array(1,2,3,5,7,2,9)))
  println(span(Array(10,20,30,12,6)))
}
