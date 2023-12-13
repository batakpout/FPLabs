package dsalgo_lab.leetcode

object Problem5_NaiveApproach extends App {
  //get all sub strings, and then check if they are plaindrome and get the largest one


  // O(n^2)
  def longestPalindromicSubString(s: String) = {
    val subStrings = for {
      i <- 0 to s.length
      j <- i + 1 to s.length
    } yield s.substring(i, j)
    val palindromeList = subStrings.filter(dsalgo_lab.strings.CheckPalindrome.checkIsPalindrome)
    println(palindromeList.maxBy(_.length))
  }

  longestPalindromicSubString("babad") // bab
  longestPalindromicSubString("cbbd")  //bb
}

object OptimalSolution extends App {
    
}
