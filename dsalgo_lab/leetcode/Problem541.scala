package dsalgo_lab.leetcode

object Problem541FP extends App {


  val k = 2
  val str = "abcd"

  val result = str.grouped(k * 2).map(s => s.take(k).reverse ++ s.drop(k)).toList.mkString
  println(result)
}

object Problem541FasterVersion extends App {

}
