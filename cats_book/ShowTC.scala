package cats_book

import java.util.Date

object ShowTC extends App {

  /**
   * trait Show[A] {
   * def show(a: A): String
   * }
   */

  import cats.Show
  import cats.instances.string._

  val showInt: Show[Int] = Show[Int]

  val res1 = showInt.show(10)

  val res2 = Show[String].show("hello")

  import cats.syntax.show._

  val res3 = 10.show
  val res4 = "world".show

  //convenient methods / construction methods
  implicit val dateInstance: Show[Date] = Show.show[Date] { d =>
    s"${d.getTime} ms since the epoch"
  }


  val res5 = new Date().show

  println(res1)
  println(res2)
  println(res3)
  println(res4)
  println(res5)

  case class Cat(name: String, age: Int, color: String)

  implicit val person: Show[Cat] = Show.show[Cat] { p =>
    val name = p.name.show
    val age = p.age.show
    val color = p.color.show
    s"$name is $age years old $color cat"
  }

  val res6 = Cat("pussy", 2, "white").show

  println(res6)

}
