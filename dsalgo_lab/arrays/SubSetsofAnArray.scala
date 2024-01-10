package dsalgo_lab.arrays

//todo:Do a backtracking solution when u pick up backtracking??

object SubSetsofAnArray extends App {

  def subSets(a: Array[Char]): Unit = {

    def subSetHelper(i: Int, remaining: Int): Unit = {
      if (remaining > 0) {
        val subSet = generateSubSet(a, i, a.length - 1)
        println(subSet)
        subSetHelper(i + 1, remaining - 1)
      }
    }

    def generateSubSet(a: Array[Char], i: Int, j: Int, s: String = ""): String = {
      if (j < 0) s else {
        if (i % 2 == 0) generateSubSet(a, i / 2, j - 1, "-" + s)
        else generateSubSet(a, i / 2, j - 1, a(j) + s)
      }
    }

    subSetHelper(0, Math.pow(2, a.length).toInt)
  }

  subSets(Array('a', 'b', 'c'))
}
