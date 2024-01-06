package dsalgo_lab.arrays

object BinarySearchImperative extends App {

  // TC: O(LogN)
  def search(arr: Array[Int], target: Int): Int = {
     def rec(start: Int = 0, end: Int = arr.length):Int = {
       if(start <= end) {
         val mid = start + ((end - start) / 2)
         if(arr(mid) == target) mid
         else if(arr(mid) > target) rec(start, end - 1)
         else rec(start + 1, end)
       } else -1
     }
    rec()
  }

  println(search(Array(1, 2, 3, 4, 5, 6, 7, 8), 8))
}
