package dsalgo_lab.arrays

import dsalgo_lab.arrays.MergeTwoSortedArrays.{arr1, arr2}
//TC : O(NLogN) // recurrence relation: T(N) = T(N/2) + T(N/2) + n
object MergeSort extends App {


  def mergSort(arr: Array[Int], low: Int, high: Int): Array[Int] = {
    if (low == high) Array(arr(low))
    else {
      val mid = (low + high) / 2
      val leftSortedArray = mergSort(arr, low, mid)
      val rightSortedArray = mergSort(arr, mid + 1, high)
      mergeTwoArrays(leftSortedArray, rightSortedArray)
    }
  }


  def mergeTwoArrays(arr1: Array[Int], arr2: Array[Int]) = {

    val arr3 = new Array[Int](arr1.length + arr2.length)
    var i = 0
    var j = 0
    var k = 0
    while (i < arr1.length && j < arr2.length) {
      if (arr1(i) < arr2(j)) {
        arr3(k) = arr1(i)
        i += 1
        k += 1
      } else {
        arr3(k) = arr2(j)
        j += 1
        k += 1
      }
    }

    while (i < arr1.length) {
      arr3(k) = arr1(i)
      i += 1
      k += 1
    }

    while (j < arr2.length) {
      arr3(k) = arr2(j)
      j += 1
      k += 1
    }
    arr3
  }

  val arr = Array(3, 2, 5, 12, 8, 1)
  val res = mergSort(Array(3, 2, 5, 12, 8, 1), 0, arr.length - 1)
  println(res.mkString("[", ",", "]"))
}
