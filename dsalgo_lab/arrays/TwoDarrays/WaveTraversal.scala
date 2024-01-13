package dsalgo_lab.arrays.TwoDarrays

//use 5 * 7 matrix to understand edge case at last
object WaveTraversal extends App {


  val m = Utils.build(4,4, 30)(Utils.f)
  Utils.display(m)
  println("----")

    for(j <- m(0).indices) {
      if(j % 2 == 0) {
        for(i <- m.indices)
          print(m(i)(j) + "\t")
      } else {
        for (i <- m.length - 1 to 0 by -1)
          print(m(i)(j) + "\t")
      }
    }
}