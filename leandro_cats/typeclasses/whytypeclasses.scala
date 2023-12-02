package leandro_cats.typeclasses

object whytypeclasses extends App {
  //using inheritance
  // Using Inheritance
  trait Equal[A] {
    def isEqual(x: A, y: A): Boolean
  }

  class IntEqual extends Equal[Int] {
    override def isEqual(x: Int, y: Int): Boolean = x == y
  }

  class StringEqual extends Equal[String] {
    override def isEqual(x: String, y: String): Boolean = x == y
  }

  // Example usage
  val intEqual = new IntEqual
  val stringEqual = new StringEqual

  println(intEqual.isEqual(5, 5)) // true
  println(stringEqual.isEqual("hello", "world")) // false

  // using type classes
  // Using Type Classes
  trait Equal2[A] {
    def isEqual(x: A, y: A): Boolean
  }

  object EqualInstances2 {
    implicit val intEqual: Equal2[Int] = (x: Int, y: Int) => x == y
    implicit val stringEqual: Equal2[String] = (x: String, y: String) => x == y
  }

  object EqualSyntax2 {
    implicit class EqualOps2[A](x: A) {
      def ===(y: A)(implicit equal: Equal2[A]): Boolean = equal.isEqual(x, y)
    }
  }

  // Example usage

  import EqualInstances2._
  import EqualSyntax2._

  println(5 === 5) // true
  println("hello" === "world") // false

  /**
     In the first approach using inheritance, we define separate classes for each type (e.g., IntEqual and StringEqual).
     This approach can lead to a combinatorial explosion of classes as the number of types and behaviors grows.
     It also introduces the closed-world assumption, where you need to modify existing classes to add new behaviors.
     In the second approach using type classes, we define a type class Equal and provide instances for specific types
     in a separate object (EqualInstances). We also define syntax for the equality operation using an
     implicit class (EqualSyntax). This allows us to add new types and behaviors without modifying existing code,
     making it open for extension.
    
     Type classes offer a more modular and extensible way to achieve polymorphism compared to traditional inheritance.
     They enable ad-hoc polymorphism, allowing you to define new behaviors for existing types without modifying their code.
     This can lead to more maintainable and scalable designs.
   */
}
