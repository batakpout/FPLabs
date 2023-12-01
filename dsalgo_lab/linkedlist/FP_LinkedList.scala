package dsalgo_lab.linkedlist_lab

import cats.syntax.all._

case class ListNode(x: Int, next: Option[ListNode])

object TestListNode extends App {

  def append(head: ListNode, d: Int): ListNode = {
    def appendRec(currentNode: ListNode): ListNode = currentNode.next match {
      case Some(next) => ListNode(currentNode.x, appendRec(next).some)
      case None => ListNode(currentNode.x, ListNode(d, None).some)
    }

    appendRec(head)
  }

  def show(currentNode: ListNode): Unit = currentNode.next match {
    case Some(next) =>
      print(currentNode.x + " -> ")
      show(next)
    case None =>
      print(currentNode.x)
  }

  val node = ListNode(1, ListNode(2, ListNode(3, None).some).some)
  show(node)
  val newNode = append(node, 4)
  println()
  show(newNode)
}