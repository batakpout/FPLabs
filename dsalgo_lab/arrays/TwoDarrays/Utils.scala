package dsalgo_lab.arrays.TwoDarrays

object Utils {

  type Row = Array[Int]
  type Matrix = Array[Row]

  def build(rowCount: Int, colCount: Int)(f: () => Int): Matrix = {
    (for (_ <- 1 to rowCount)
      yield (for (_ <- 1 to colCount)
        yield f()).toArray).toArray
  }

  def display(arr: Matrix):Unit = {
    arr.foreach { row =>
      row.foreach { elem =>
        print(elem + "\t")
      }
      println()
    }
  }

  def buildUserInput(rowCount: Int, colCount: Int) = {
    val arr: Matrix = Array.ofDim[Int](rowCount, colCount)
    for(i <- 0 until rowCount) {
      for(j <- 0  until colCount)
         arr(i)(j) = scala.io.StdIn.readInt()
    }
  }

  import scala.util.Random
  val f = () => new Random().nextInt(10) + 1

}
