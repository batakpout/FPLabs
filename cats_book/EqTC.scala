package cats_book

import java.util.Date

object EqTC extends App {
  /*
    trait Eq[A] {
     def eqv(a: A, b: A): Boolean
    }
   */

  import cats.Eq
  import cats.instances.int._

  val evInt = Eq[Int]
  val res1 = evInt.eqv(10, 20)
  val res2 = Eq[String].eqv("hello", "hello")

  //val res3 = Eq[Double].eqv("hello", 23.1) CTE

  import cats.syntax.eq._

  val res3 = 123 === 222
  val res4 = 123 =!= 122

  // val err = "123" === 124 : CTE

  import cats.instances.option._
  // val res5 = Some(1) === None // CTE: types doesn't quite match ip

  val res6 = (Some(1): Option[Int]) === (None: Option[Int])
  val res7 = Option(1) === Option.empty[Int]

  import cats.syntax.option._

  val res8 = 1.some === none[Int]
  val res9 = 1.some =!= none[Int]

  import cats.instances.long._

  implicit val res10: Eq[Date] = Eq.instance[Date]((d1, d2) =>
    d1.getTime === d2.getTime
  )


  val d1 =  new Date()
  val d2 =  new Date()

  val res11 = res10.eqv(d1, d2)
  val res12 = res10.neqv(d1, d2)

  val res13 = d1 === d2
  val res14 = d1 =!= d2



  println(res1)
  println(res2)
  println(res3)
  println(res4)
  println(res6)
  println(res7)
  println(res8)
  println(res9)
  println(res10)
  println(res11)
  println(res12)
  println(res13)
  println(res14)

  case class Cat(name: String, age: Int, color: String)

  implicit val eqCat: Eq[Cat] = Eq.instance[Cat]{ (c1, c2) =>
    (c1.name === c2.name) && (c1.age == c2.age) && (c1.color == c2.color)
  }

  val c1 = Cat("pussy", 2, "white")
  val c2 = Cat("kitty", 2, "white")

  val res15 = c1 === c2
  val res16 = c1 =!= c2

  println(res15)
  println(res16)

  val optionC1: Option[Cat] = Option(c1)

  val res17 = optionC1 === Option.empty[Cat]
  val res18 = optionC1 =!= Option.empty[Cat]
  val res19 = c2.some === none[Cat]

  println(res17)
  println(res18)
  println(res19)

}
