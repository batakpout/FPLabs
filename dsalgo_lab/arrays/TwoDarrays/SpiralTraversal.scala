package dsalgo_lab.arrays.TwoDarrays

object SpiralDisplay extends App {

    val rows = 5
    val cols = 7

   val m = Utils.build(rows, cols, 60)(Utils.f)
   Utils.display(m)
   println("------")

   var minC = 0
   var minR = 0
   var maxC = m(0).length - 1
   var maxR = m.length - 1

   val total = rows * cols
   var count = 0
   var i = 0
   var j = 0
   while(count < total) {

     //left vertical tour
     j = minC
     for {
       i <- minR to maxR
       if count < total
     } {
       count += 1
       print(m(i)(j) + "\t")
     }

     minC += 1
    // bottom horizontal tour
     i = maxR
     for {
       j <- minC to maxC
       if count < total
     } {
       count += 1
       print(m(i)(j) + "\t")
     }

     maxR -= 1
     // right vertical tour
     j = maxC
     for {
       i <- maxR to minR by -1
       if count < total
     } {
       count += 1
       print(m(i)(j) + "\t")
     }

     maxC -= 1
     // top horizontal tour
     i = minR
     for {
       j <- maxC to minC by -1
       if count < total
     } {
       count += 1
       print(m(i)(j) + "\t")
     }
     minR += 1

   }

}