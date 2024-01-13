package dsalgo_lab.arrays.TwoDarrays

object MatrixMultiply extends App {
  /**
    second matrix's row should be equal to columns of first matrix
    final matrix will have rows equals to first matrix and cols equal to second matrix
   */
    val row1 = 2
    val col1 = 3
    val row2 = 3
    val col2  = 4

   val matrix1 = Utils.build(row1, col1)(Utils.f)
   val matrix2 = Utils.build(row2, col2)(Utils.f)

   val resultMatrix = Array.ofDim[Int](row1, col2)

   for(i <- resultMatrix.indices) {
     for( j <- resultMatrix(i).indices) {
       for(k <- 0 until col1) {
         resultMatrix(i)(j) +=  matrix1(i)(k) * matrix2(k)(j)
       }
     }
   }

  Utils.display(matrix1)
  println("--------")
  Utils.display(matrix2)
  println("--------")
  Utils.display(resultMatrix)


}
