package dsalgo_lab.leetcode

//
object NaiveApproach extends App {
 //copy arr1 and arr2 to arr3, then sort it , and then use N/2 (if odd size arry) else [N/2 + N/2 - 1] / 2
  //Auxiliary Space: O(N + M), Creating a new array of size N+M.
  // TC : O((N + M) * Log (N + M)) , sort complexity, NlogN of merge sort

}

object OptimizedApproach extends App {
  //TC: O(LogN) -- uses binary search

  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {

    if (nums1.length > nums2.length)
      findMedianSortedArrays(nums2, nums1) else {
      val N = nums1.length + nums2.length
      val n1 = nums1.length
      val n2 = nums2.length

      def search(start: Int, end: Int): Double = {
        if (start > end) 0.0 else {
          val cut1 = start + (end - start) / 2
          val cut2 = N / 2 - cut1

          val l1 = if (cut1 == 0) Integer.MIN_VALUE else nums1(cut1 - 1)
          val l2 = if (cut2 == 0) Integer.MIN_VALUE else nums2(cut2 - 1)
          val r1 = if (cut1 == n1) Integer.MAX_VALUE else nums1(cut1)
          val r2 = if (cut2 == n2) Integer.MAX_VALUE else nums2(cut2)

          if (l1 <= r2 && l2 <= r1) {
            if (N % 2 == 0)
              (Math.max(l1, l2) + Math.min(r1, r2)) / 2.0
            else Math.min(r1, r2)
          } else {
            if (l1 > r2) search(start, cut1 - 1)
            else search(cut1 + 1, end)
          }
        }
      }
      search(0, nums1.length)
    }
  }

  val result = findMedianSortedArrays(Array(3,4), Array(1,2))
  println(result)
}
