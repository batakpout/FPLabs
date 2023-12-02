package dsalgo_lab.leetcode

import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
 * 1. Naive solution to use two loops and check for target
 * Time Complexity: O(N2), Finding pair for every element in the array of size N.
 * Auxiliary Space: O(1)
 *
 * 2. Using Sorting and Two-pointer technique [sort first, then check a[left] + a[left] = target else move left or right if > or <
 * Time Complexity: O(NlogN), Time complexity for sorting the array
 * Auxiliary Space: O(1)
 *
 * 3. Hashing: Time: O(n), space : O(n)
 */
object Problem1 extends App {

  def checkSum(arr: Array[Int], target: Int): Array[(Int, Int)] = {
    val hashMap = HashMap[Int, Int]()

    def rec(map: HashMap[Int, Int], index: Int): Array[(Int, Int)] = {
      if (index >= arr.length) Array()
      else {
        if (map.contains(arr(index))) Array((map(arr(index)), index))
        else rec(map + ((target - arr(index)) -> index), index + 1)
      }
    }

    rec(hashMap, 0)
  }

  checkSum(Array(1, 2, 3, 4, 5), 8).foreach(println)
}
