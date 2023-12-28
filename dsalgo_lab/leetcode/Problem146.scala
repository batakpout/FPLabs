package dsalgo_lab.leetcode
// Difficulty: Medium
//LRU cache
//The functions get and put must each run in O(1) average time complexity.
object Problem146 extends App {

    class LRUCache(capacity: Int) {

      import scala.collection.mutable

      case class Node(var prev: Node, var next: Node, key: Int, value: Int)

      private val head: Node = Node(null, null, 0, 0)
      private val tail: Node = Node(null, null, 0, 0)
      private val map = mutable.HashMap[Int, Node]()

      head.next = tail
      tail.prev = head

      def get(key: Int): Int = {
        if (map.contains(key)) {
          val node = map(key)
          remove(node)
          insert(node)
          node.value
        } else -1
      }

      def put(key: Int, value: Int): Unit = {
        if (map.contains(key)) remove(map(key))
        if (map.size == capacity) remove(tail.prev)
        insert(Node(null, null, key, value))
      }

      def remove(node: Node): Unit = {
        map -= node.key
        node.prev.next = node.next
        node.next.prev = node.prev
      }

      def insert(node: Node): Unit = {
        map += (node.key -> node)
        node.next = head.next
        node.prev = head
        head.next = node
        node.next.prev = node
      }

      def display(): Unit = {
        var current = head.next
        while (current != tail) {
          println(s"${current.key} : ${current.value}")
          current = current.next
        }
        println("----")
      }
    }

    val capacity = 3
    val obj = new LRUCache(capacity)
    obj.put(1, 1)
    obj.put(2, 2)
    obj.put(3, 3)
    obj.display()
    println(obj.get(1)) // Output: 1
    obj.display()
    obj.put(4, 4)
    obj.display()
    println(obj.get(2)) // Output: -1
}
