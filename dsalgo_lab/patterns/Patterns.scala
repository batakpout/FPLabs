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

/**
  1
  1 2
  1 2 3
  1 2 3 4
  1 2 3 4 5
 */

object Pattern11 extends App {
    def printPattern(n: Int): Unit = {
      for (i <- 1 to n) {
        for (j <- 1 to i) {
          print(s"$j\t")
        }
        println()
      }
    }

    printPattern(10)

}

/**
  0
  1 1
  2 3 5
  8 13 21 34
 */

object Pattern12 extends App {

  def printPattern(n: Int): Unit = {
    var curr = 0
    var next = 1
    for (i <- 1 to n) {
      for (_ <- 1 to i) {
        print(s"$curr\t")
        val r = curr + next
        curr = next
        next = r
      }
      println()
    }
  }

  printPattern(5)

}

/**
  1
  1 1
  1 2 1
  1 3 3 1
  1 4 6 4 1
 */
//todo: solve using DP
object Pattern13 extends App {

  /**
     C(i, j + 1) = C(i, j) * (i - j) / j + 1
   */

  def pascalTriangle(n: Int) = {
    for (i <- 0 until n) {
      var iCj = 1
      for (j <- 0 to i) {
        print(s"$iCj\t")
        iCj = (iCj * (i - j)) / (j + 1)
      }
      println()
    }
  }

  pascalTriangle(10)
}

/**
  multiplication table:
  5 * 1 = 5
  5 * 2 = 10
  ...........
 */
object Pattern14 extends App {

  def mulTable(n: Int) = {
    for (i <- 1 to 10) {
      val res = n * 1
      print(s"$n * $i = $res")
      println()
    }
  }
  mulTable(5)
}

/**
  bit difficult than others
         1
      2  3  2
   3  4  5  4  3
      2  3  2
         1
 */

object Pattern15 extends App {

  def printPattern(n: Int):Unit = {
    var st = 1
    var sp = n / 2
    var r = 1
    for(i <- 1 to n) {

       for(_<- 1 to sp) {
         print("\t")
       }

      var c = r
       for(j <- 1 to st) {
        print(c + "\t")

         if(j <= st/2) c+=1 else c -=1
      }

      if(i <= n/ 2) {
        sp -= 1
        st += 2
        r += 1
      } else {
        sp += 1
        st -= 2
        r -= 1
      }
      println()
    }
  }
  printPattern(5)
}

/**
  1          1
  1 2      2 1
  1 2 3   3 2 1
  1 2 3 4 3 2 1
 */
object Pattern16 extends App {

  def printPattern(n: Int): Unit = {
    var sp = 2 * n - 3
    var st = 1
    for(i <- 1 to n) {

      var r = 1
      for(_<- 1 to st) {
        print(r + "\t")
        r += 1
      }

      for(_ <- 1 to sp) {
        print("\t")
      }

      if(i == n) {
        st -= 1
        r -= 1
      }

      r -= 1
      for (_ <- 1 to st) {
        print(r + "\t")
        r -= 1
      }

      sp -= 2
      st += 1
      println()

    }
  }
  printPattern(5)
}

/**
          *
          * *
      * * * * *
          * *
          *
 */
object Pattern17 extends App {

    def printPattern(n: Int): Unit = {

      var sp = n / 2
      var st = 1

      for (i <- 1 to n) {

        for (_ <- 1 to sp) {
          if (i == n / 2 + 1)
            print("*\t")
          else print("\t")
        }

        for (_ <- 1 to st) {
          print("*\t")
        }
        if (i <= n / 2) st += 1 else st -= 1
        println()
      }
    }

    printPattern(5) // odd input

}

object Pattern18 extends App {

  def printPattern(n: Int): Unit = {
     var sp = 0
     var st = n
    for(i <- 1 to n) {

       for(_ <- 1 to sp) {
         print("\t")
       }

      for(j <- 1 to st) {
         if(i > 1 && i <= n/2 && j > 1 && j < st)
           print("\t")
         else print("*\t")
      }

      if(i <= n/2) {
        sp += 1
        st -= 2
      } else {
        sp -=1
        st += 2
      }
      println()
    }
  }
  printPattern(7)
}
