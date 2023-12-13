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
    //make changes to it 
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
