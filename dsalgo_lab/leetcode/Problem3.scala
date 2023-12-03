package dsalgo_lab.leetcode

import scala.collection.mutable


object Problem3 extends App {

  // O(n^2)
  def maxSubString(s: String) = {
    val subStrings = for {
      i <- 0 to s.length
      j <- i + 1 to s.length
    } yield s.substring(i, j)
    val uniqueCharSubStrings = subStrings.filter(x => x.toSet.size == x.length)
    println(uniqueCharSubStrings.maxBy(_.length))
  }

  maxSubString("pwwkew")
}

// TC: O(n)
object SlidingWindow extends App {

  import scala.annotation.tailrec

  def calculateFP(s: String): Int = {
    @tailrec
    def calculateTailRec(l: Int, r: Int, set: Set[Char], max: Int): Int = {
      if (r >= s.length) max
      else {
        val c = s.charAt(r)
        if (set.contains(c)) {
          @tailrec
          def removeItemsFromSet(ss: Set[Char], l: Int): (Int, Set[Char]) = {
            if (s.charAt(l) != c)
              removeItemsFromSet(ss - s.charAt(l), l + 1)
            else
              (l + 1, ss - c)
          }
          val (newLeft, newSet) = removeItemsFromSet(set, l)
          calculateTailRec(newLeft, r, newSet, max)
        } else {
          calculateTailRec(l, r + 1, set + c, Math.max(max, r - l + 1))
        }
      }
    }
    calculateTailRec(0, 0, Set.empty, 0)
  }


  def calculate(s: String): Int = {
      var l = 0
      var r = 0
      val set = new scala.collection.mutable.HashSet[Int]()
      var max = 0

      while(r < s.length) {
        val c = s.charAt(r)
        if(set.add(c)) {
           max = Math.max(max, r - l + 1)
          r += 1
        } else {
          while(s.charAt(l) != c) {  //abcacbbb
            set.remove(s.charAt(l))
            l += 1
          }
          set.remove(c)
          l += 1
        }
      }
      max
    }
 // println(calculate("abcacbbbb"))
  //println(calculate("bbbbbb"))
 // println(calculate("pwwkew"))
}
