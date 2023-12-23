/**
  input arr = [3,1,0,7,5]
            *
            *
            * *
            * *
     *      * *
     *      * *
     * *    * *

 */
object BarChat extends App {

  //TC: O(N) for finding max + O(N^2) for building chart

  def buildBarChat(arr: Array[Int]): Unit = {
    def findMax(arr: Array[Int], index: Int, max: Int): Int = {
      if(index < arr.length) {
         if(max > arr(index)) findMax(arr, index + 1, arr(index))
         else findMax(arr, index + 1, max)
      } else max
    }

    val max = findMax(arr, 1, arr(0))

    def build(arr: Array[Int], max: Int):Unit = {
      if(max == 0) () else {
          def printBar(arr: Array[Int], index: Int): Unit = {
            if(index < arr.length) {
              if(arr(index) >= max) print("*\t")
              else print("\t")
            } else ()
          }
        printBar(arr, 0)
        build(arr, max - 1)
      }
    }
    build(arr, max)
  }

  buildBarChat(Array(3,1,0,7,5))
}
