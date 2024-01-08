package dsalgo_lab.arrays

object SubArrays extends App {
 //TC: O(N^2 or N^3))
  /**
   The third loop (innermost) does not contribute to a significant change in the overall time complexity, 
   as it is dominated by the first two loops.
   */
 //todo: find an optimal solution?
    def subArrays(a: Array[Char]): Unit = {
       for(i <- 0 until a.length) {
         for(j <- i until a.length) {
           for(k <- i to j) {
             print(a(k))
           }
           println()
         }
       }
    }

   subArrays(Array('a', 'b', 'c'))

}
