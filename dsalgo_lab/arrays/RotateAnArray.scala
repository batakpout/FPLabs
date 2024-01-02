package dsalgo_lab.arrays

object RotateAnArray extends App {

  def reverse(arr: Array[Int], x: Int, y: Int) = {
    var i = x
    var j = y
    var temp = 0
    while (i < j) {
      temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
      i += 1
      j -= 1
    }
  }

  def rotate(arr: Array[Int], k: Int) = {
    val l = arr.length
    var b = k % l
    b = if (b < 0) l + b else b

    reverse(arr, 0, l - b - 1)
    reverse(arr, l - b, l - 1)
    reverse(arr, 0, l - 1)
    println(arr.mkString("[", ",", "]"))
  }
  rotate(Array(-1, -100, 3, 99), 2)

}

object ListRotationFP extends App {
  def rotateList[A](list: List[A], positions: Int): List[A] =  list match {
    case Nil => list
    case _ =>
      val (front, back) = list.splitAt(list.length - positions)
      back ++ front
    }

  val rotatedList = rotateList( List(1,2,3,4,5,6), 4)
  println(rotatedList)
}
