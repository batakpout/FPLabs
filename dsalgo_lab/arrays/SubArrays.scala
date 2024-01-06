package dsalgo_lab.arrays

object SubArrays extends App {

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
