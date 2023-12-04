package dsalgo_lab.arrays

object MergeTwoSortedArrays extends App {

  //TC O(N + M)
  val arr1 = Array(5, 8, 9, 11, 21)
  val arr2 = Array(4, 6, 10, 14)

  val arr3 = new Array[Int](arr1.length + arr2.length)
  println(arr3.length)

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

  println(arr3.mkString("[", ",", "]"))
}
