package dsalgo_lab.arrays

object MergeSortedArrays extends App {

 //O(N * LogN)
  def mergeSort(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case h :: Nil => List(h)
    case _ =>
      val (left, right) = list.splitAt(list.length / 2)
      merge(mergeSort(left), mergeSort(right))
  }

  def merge(left: List[Int], right: List[Int]): List[Int] =
    (left, right) match {
      case (_, Nil) => left
      case (Nil, _) => right
      case (leftHead :: leftTail, rightHead :: rightTail) =>
        if (leftHead < rightHead) leftHead :: merge(leftTail, right)
        else rightHead :: merge(left, rightTail)
    }

  println {
    mergeSort(List(6, 5, 4, 3, 2, 1))
  }
}
