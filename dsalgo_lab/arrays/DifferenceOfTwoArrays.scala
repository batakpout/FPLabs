package dsalgo_lab.arrays

import scala.util.control.Breaks.{break, breakable}

object DifferenceOfTwoArrays extends App {

  def diff(a1: Array[Int], a2: Array[Int]): Unit = {
    val n1 = a1.length
    val n2 = a2.length

    if (n2 < n1) diff(a2, a1)

    val a3 = new Array[Int](n2)
    var i = n2 - 1
    var j = n1 - 1
    var k = a3.length - 1
    var c = 0

    while (k > 0) {

      val a1Value = if (j >= 0) a1(j) else 0

      if (a2(i) + c >= a1Value) {
        a3(k) = a2(i) + c - a1Value
        c = 0
      } else {
        a3(k) = (a2(i) + c + 10) - a1Value
        c = -1
      }
      i -= 1
      j -= 1
      k -= 1
    }

    var idx = 0
    breakable {
      while (idx < a3.length) { //// e.g 1000 - 1 = 0999
        if (a3(idx) == 0) idx += 1
        else break
      }
    }

    while (idx < a3.length) {
      print(a3(idx) + " ")
      idx += 1
    }
  }

  diff(Array(9, 9, 9, 9, 9), Array(3, 3, 3, 3, 3, 3))
}

object FPVersion extends App {

  //assume l1 is greater than l2
  
  

  def difference(l1: List[Int], l2: List[Int], result: List[Int] = Nil, c: Int = 0): List[Int] = (l1, l2) match {
    case (Nil, Nil) => result.dropWhile(_ == 0)
    case (list1, Nil) =>
      val n1 = list1.last
      if (n1 + c >= 0) difference(list1.init, Nil, (n1 + c) :: result, 0)
      else difference(list1.init, Nil, ((n1 + c + 10)) :: result, -1)
    case (Nil, _) => result.dropWhile(_ == 0)
    case (list1, list2) =>
      val n1 = list1.last
      val n2 = list2.last
      if (n1 + c >= n2) difference(list1.init, list2.init, ((n1 + c) - n2) :: result, 0)
      else difference(list1.init, list2.init, ((n1 + c + 10) - n2) :: result, -1)

  }

  println(difference(List(3, 3, 3, 3, 3, 3), List(9, 9, 9, 9, 9)))
  println(difference(List(1, 0, 0, 0), List(1)))
}
