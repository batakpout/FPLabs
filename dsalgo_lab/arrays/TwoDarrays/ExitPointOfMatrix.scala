package dsalgo_lab.arrays.TwoDarrays

import dsalgo_lab.arrays.TwoDarrays.Utils.display

object ExitPointOfAMatrix extends App {

  val a1 = Array(
    Array(0, 0, 1, 0),
    Array(1, 0, 0, 0),
    Array(0, 0, 0, 0),
    Array(1, 0, 1, 0)
  )

  val a = Utils.build(4,4,2)(Utils.f)
  display(a)
  println("-----")

  import Utils._

   //dir = 0 - east, 1 - south, 2 - west, 3 - north
  def exitPoint(a: Matrix, i: Int, j: Int, dir: Int): (Int, Int) = {
    if (i == -1) (i + 1, j)
    else if (j == -1) (i, j + 1)
    else if (i == a.length) (i - 1, j)
    else if (j == a(0).length) (i, j - 1)
    else {
      val newDir = dir + a(i)(j)
      if (newDir % 4 == 0) exitPoint(a, i, j + 1, newDir)
      else if (newDir % 4 == 1) exitPoint(a, i + 1, j, newDir)
      else if (newDir % 4 == 2) exitPoint(a, i, j - 1, newDir)
      else exitPoint(a, i - 1, j, newDir)
    }
  }

  println(exitPoint(a, 0, 0, 0))
}
