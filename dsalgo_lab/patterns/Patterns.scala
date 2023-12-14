package dsalgo_lab.patterns

/**
 *
 * *
 * * *
 * * * *
 * * * * *
 */
object Pattern1 extends App {

  def printPattern(n: Int): Unit = {
    for (i <- 1 to n) {
      for (_ <- 1 to i) {
        print("*\t")
      }
      println()
    }
  }

  printPattern(10)
}

/**
  * * * * *
  * * * *
  * * *
  * *
  *
 */
object Pattern2 extends App {

  def printPattern(n: Int): Unit = {
    for (i <- 1 to n) {
      for (_ <- i to n) {
        print("*\t")
      }
      println()
    }
  }

  printPattern(10)
}

/**
            *
         *  *
      *  *  *
   *  *  *  *
 * *  *  *  *
 */

object Patterns3 extends App {

   def printPatterns(n: Int): Unit = {
     var sp = n - 1
     var st = 1
     for (_ <- 1 to n) {

         for(_ <- 1 to sp) {
            print("\t")
         }

        for (_ <- 1 to st) {
         print("*\t")
       }
            sp -= 1
            st += 1
            println()

     }
   }
  printPatterns(10)
}

/**
   * * * * *
     * * * *
        * * *
          * *
            *
 */

object Patterns4 extends App {

  def printPatterns(n: Int): Unit = {
    var sp = 0
    var st = n
    for (_ <- 1 to n) {

      for(_ <- 1 to sp) {
        print("\t")
      }

      for (_ <- 1 to st) {
        print("*\t")
      }
      sp += 1
      st -= 1
      println()

    }
  }
  printPatterns(10)
}

/**
         *
      *  *  *
   *  *  *  *  *
      *  *  *
         *
 */
object Patterns5 extends App {

  def printPatterns(n: Int): Unit = {
    var sp = n / 2
    var st = 1
    for (i <- 1 to n) {

      for(_ <- 1 to sp) {
        print("\t")
      }

      for (_ <- 1 to st) {
        print("*\t")
      }

      if(i <= n/2) {
        sp -= 1
        st += 2
      } else {
        sp += 1
        st -=2
      }

      println()

    }
  }
  printPatterns(10)
}

/**
   * * *  * * *
   * *      * *
   *          *
   * *      * *
   * * *  * * *
 */

object Patterns6 extends App {

  def printPatterns(n: Int): Unit = {
    var sp = 1
    var st = n / 2 + 1
    for (i <- 1 to n) {

      for (_ <- 1 to st) {
        print("*\t")
      }

      for(_ <- 1 to sp) {
        print("\t")
      }

      for (_ <- 1 to st) {
        print("*\t")
      }

      if(i <= n/2) {
        sp += 2
        st -= 1
      } else {
        sp -= 2
        st +=1
      }

      println()

    }
  }
  printPatterns(10)
}

/**
  *
     *
       *
          *
             *
 */
object Pattern7 extends App {

  def printPattern(n: Int): Unit = {
    for(i <- 1 to n) {
       for(j <- 1 to n) {
          if(i == j) print("*\t")
          else print("\t")
       }
      println()
    }
  }

  printPattern(5)

}

/**
          *
        *
      *
    *
  *
 */
object Pattern8 extends App {

  def printPattern(n: Int): Unit = {
    for (i <- 1 to n) {
      for (j <- 1 to n) {
        if (i + j == n + 1)
          print("*\t")
        else print("\t")
      }
      println()
    }
  }

  printPattern(5)

}

/**
  *       *
    *   *
      *
    *   *
  *      *
 */

object Pattern9 extends App {
  def printPatterns(n: Int): Unit = {
    for (i <- 1 to n) {
      for (j <- 1 to n) {
        if (i + j == n + 1 || i == j)
          print("*\t")
        else print("\t")
      }
      println()
    }
  }
  printPatterns(5)
}

/**
       *
    *    *
 *          *
    *    *
       *
 */

object Pattern10 extends App {

  def printPatterns(n: Int): Unit = {
    var os = n / 2
    var is = -1

    for (i <- 1 to n) {

       for(_ <- 1 to os) {
         print("\t")
       }

      print("*\t")

      for (_ <- 1 to is) {
        print("\t")
      }

      if(i > 1 && i < n)
        print("*\t")

      if(i <= n/2) {
        os -= 1
        is += 2
      } else {
        os += 1
        is -= 2
      }
      println()
    }
  }

  printPatterns(5)
}
