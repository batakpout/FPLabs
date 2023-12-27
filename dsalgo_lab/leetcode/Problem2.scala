package dsalgo_lab.leetcode
//ADD TWO NUMBERS USING LINKED LIST
//Difficulty: Medium
//Time: O(N), Space: O(1)
object Problem2 extends App {


  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def show(head: ListNode): Unit = {
    var currentNode = head
    while (currentNode != null) {
      print(currentNode.x + " -> ")
      currentNode = currentNode.next
    }
    print("null")
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    var list1 = l1
    var list2 = l2
    var carry = 0
    val dummyNode = new ListNode(0)
    var currentNode = dummyNode

    while (list1 != null || list2 != null || carry > 0) {

      if (list1 != null) {
        carry = carry + list1.x
        list1 = list1.next
      }

      if (list2 != null) {
        carry = carry + list2.x
        list2 = list2.next
      }
      val node = new ListNode(carry % 10)
      carry = carry / 10
      currentNode.next = node
      currentNode = node
    }

    dummyNode.next
  }

  val l1 = new ListNode(9, new ListNode(9, new ListNode(9, new ListNode(9, new ListNode(9, new ListNode(9, new ListNode(9)))))))
  val l2 = new ListNode(9, new ListNode(9, new ListNode(9, new ListNode(9))))
  val res = addTwoNumbers(l1, l2)
  show(res)

}
