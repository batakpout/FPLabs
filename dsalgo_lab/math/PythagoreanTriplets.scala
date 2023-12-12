package dsalgo_lab.math

object PythagoreanTriplet extends App {

  def arePythagoreanTriplets(n1: Int, n2: Int, n3: Int): Boolean = {
    val max: Int = {
      if (n1 > n2 && n1 > n3) n1
      else if (n2 > n3) n2
      else n3
    }
    if (n1 == max) (n1 * n1) == (n2 * n2) + (n3 * n3)
    else if (n2 == max) (n2 * n2) == (n1 * n1) + (n3 * n3)
    else (n3 * n3) == (n2 * n2) + (n1 * n1)
  }

  println(arePythagoreanTriplets(1,2,3))
  println(arePythagoreanTriplets(5,3,4))
  println(arePythagoreanTriplets(6,5,4))
}
