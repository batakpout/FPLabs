package dsalgo_lab.arrays

object FloorAndCeil_BrokenEconomy extends App {

  val a = Array(10, 20, 20, 30, 50, 60, 70, 70, 80) //sorted array

  def ceilFloor(arr: Array[Int], target: Int): (Int, Int) = {
    //calculate Min Max of array and then check if target is < Min or > Max then decide?
    def rec(start: Int = 0, end: Int = arr.length,
            floor: Int = Integer.MIN_VALUE, ceil: Int = Integer.MAX_VALUE): (Int, Int) = {
      if (start <= end) {
        val mid = start + ((end - start) / 2)
        if (target > arr(mid))
          rec(mid + 1, end, arr(mid), ceil)
        else if (target < arr(mid))
          rec(start,mid - 1, floor, arr(mid))
        else (arr(mid), arr(mid))
      } else (floor, ceil)
    }

    rec()
  }

  println(ceilFloor(a, 62))
  println(ceilFloor(a, 21))
  println(ceilFloor(a, 71))
  println(ceilFloor(a, 30))
  println(ceilFloor(a, 20))
}
