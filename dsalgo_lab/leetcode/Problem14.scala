package dsalgo_lab.leetcode
/*
   Longest Common Prefix
   TC:
   Difficulty: Easy

   Input: strs = ["flower","flow","flight"]
   Output: "fl"

 */
object Problem14 extends App {

    def solutionNaive(s: Array[String]): String = {

        def check(s1: String, s2: String, i: Int = 0, res: String = ""):String = {
          if(i < s1.length && i < s2.length) {
            if(s1.charAt(i)== s2.charAt(i))
              check(s1, s2, i + 1, res + s1.charAt(i))
            else res
          } else res
        }

      var common = s(0)

      for(i <- 1 until s.length) {
        common = check(s(i), common)
      }

     common
    }

  println(solutionNaive(Array("dalakabog","dalakababecar","dalcar")))
}
