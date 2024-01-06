package dsalgo_lab.math

object BenjaminBulbs extends App {

  // all perfect squares have odd number of factors, so they will be ON after N fluctuations
  def checkOnBulbsAfterNFluctuations(n: Int): List[Int] = {
    def check(i: Int = 1, list: List[Int] = Nil): List[Int] = {
      if (i * i > n) list
      else check(i + 1, (i * i) :: list)
    }

    check()
  }

  println(checkOnBulbsAfterNFluctuations(40).reverse)
}

/**
  A perfect square has odd number of factors:
  e.g 9 = 1, 3, 4
  e.g 36 = 1,2,3,4,6,9,36
  So if we have odd number of factors then that bulb will be on
 */
