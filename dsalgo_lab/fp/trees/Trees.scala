package dsalgo_lab.fp

sealed trait BinTree[+A]
case object Leaf extends BinTree[Nothing]
case class Branch[A](value: A, left: BinTree[A], right: BinTree[A]) extends BinTree[Nothing]
object Trees1 extends App {

  /**
   - a balanced binary tree i.e balanced tree's left and right sub- tress have almost equal number of nodes.
   */
  def buildTree[A](list: List[A]): BinTree[A] = list match {
    case Nil => Leaf
    case x :: xs =>
      val k = xs.length / 2
      Branch(x, buildTree(xs.take(k)), buildTree(xs.drop(k)))
  }

  val tree = buildTree(List(1,2,3,4,5,6,7,8))
  println(tree)

  /** the size method computes the no of non-leaf nodes in a binary tree*/

  def size[A](tree: BinTree[A]): Int = tree match {
    case Leaf => 0
    case Branch(_, l, r) => 1 + size(l) + size(r)
  }

  /** depth of a tree is the length of the longest path from a root to a leaf*/

  def depth[A](tree: BinTree[A]): Int = tree match {
    case Leaf => 0
    case Branch(_, l, r) => 1 + (depth(l) max depth(r))
  }

  println(s"size of tree: ${size(tree)}")
  println(s"depth of tree: ${depth(tree)}")

  /**
    if binary tree has a size(tree) = 2 ^^ (depth(tree)) - 1, then it is a complete binary tree
   */

}
