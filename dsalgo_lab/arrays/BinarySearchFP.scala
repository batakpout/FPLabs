package dsalgo_lab.arrays

object BinarySearchImperative extends App {

  // TC: O(LogN)
  def search(arr: Array[Int], target: Int, start: Int, end: Int): Int = {
    if (start > end) -1 else {
      val mid = start + ((end - start) / 2)
      arr(mid) match {
        case i if i == target => mid
        case i if i > target => search(arr, target, start, mid - 1)
        case i if i < target => search(arr, target, mid + 1, end)
      }
    }
  }

  println(search(Array(1, 2, 3, 4, 5, 6, 7, 8), 8, 0, 7))
}
