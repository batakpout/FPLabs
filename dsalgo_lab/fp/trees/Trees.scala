package dsalgo_lab.fp

sealed trait BinTree[+A]

case object Leaf extends BinTree[Nothing]

case class Branch[A](value: A, left: BinTree[A], right: BinTree[A]) extends BinTree[Nothing]

/**
  A binary tree is a tree in which each has has zero, one, or at most two child nodes
 */
object Trees1 extends App {

  /**
    - a balanced binary tree i.e balanced tree's left and right sub-tress have almost equal number of nodes.
   */
  def buildTree[A](list: List[A]): BinTree[A] = list match {
    case Nil => Leaf
    case x :: xs =>
      val k = xs.length / 2
      Branch(x, buildTree(xs.take(k)), buildTree(xs.drop(k)))
  }

  val tree = buildTree(List(1, 2, 3, 4, 5, 6, 7, 8))
  println(tree)

  /** the size method computes the no of non-leaf nodes in a binary tree */

  def size[A](tree: BinTree[A]): Int = tree match {
    case Leaf => 0
    case Branch(_, l, r) => 1 + size(l) + size(r)
  }

  /** depth of a tree is the length of the longest path from a root to a leaf */

  def depth[A](tree: BinTree[A]): Int = tree match {
    case Leaf => 0
    case Branch(_, l, r) => 1 + (depth(l) max depth(r))
  }

  println(s"size of tree: ${size(tree)}")
  println(s"depth of tree: ${depth(tree)}")

  /**
   * if binary tree has a size(tree) = 2 ^^ (depth(tree)) - 1, then it is a complete binary tree
   */

  /** Building a complete binary tree */

  def buildCompleteTree[A](d: Int, v: Int): BinTree[A] = {
    if (d == 0) Leaf
    else Branch(v, buildCompleteTree(d - 1, v * 2), buildCompleteTree(d - 1, v * 2 + 1))
  }

  val completeTree = buildCompleteTree(3, 1)
  println(completeTree)

    /** check if two Binary trees are equal */

   def equal[A](tree1: BinTree[A], tree2: BinTree[A]): Boolean = (tree1, tree2) match {
     case (Leaf, Leaf) => true
     case (Branch(v1, l1, r1), Branch(v2, l2, r2)) if v1 == v2 =>
       equal(l1, l2) && equal(r1, r2)
     case _ => false
   }

  /** flip a binary tree, root same, branches left to right */
   def flip[A](tree: BinTree[A]): BinTree[A] = tree match {
     case Leaf => Leaf
     case Branch(v, l, r) => Branch(v, flip(r), flip(l))
   }

  val flipped = flip(completeTree)
  println(flipped)

  /** flipEqual to check if t1 is flipped version of t2*/

  def flippedEqual[A](tree1: BinTree[A], tree2: BinTree[A]): Boolean = (tree1, tree2) match {
    case (Leaf, Leaf) => true
    case (Branch(v1, l1, r1), Branch(v2, l2, r2)) if v1 == v2 =>
      flippedEqual(l1, r2) && flippedEqual(r1, l2)
    case _ => false
  }

  println(flippedEqual(completeTree, flipped))
  println(flippedEqual(completeTree, completeTree))


}
