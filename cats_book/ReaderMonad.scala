package cats_book

import cats.Id
import cats.data.Kleisli

object ReaderMonad1 extends App {

  final case class Cat(name: String, favoriteFood: String)

  import cats.data.Reader

  val catName: Reader[Cat, String] = Reader.apply(cat => cat.name)

  val f1: Cat => Id[String] = catName.run
  val r1 = f1(Cat("Garfield", "lasagne"))
  val r2 = catName(Cat("Garfield", "lasagne"))
  println(r1)
  println(r2)

  println("-" * 10)

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello $name")

  val f2: Cat => Id[String] = greetKitty.run
  val r3 = f2(Cat("Healthcliff", "junk food"))

  println(r3)

  val feedKitty: Reader[Cat, String] = Reader(cat =>
    s"Have a nice bowl of ${cat.favoriteFood}"
  )

  //val greetAndFeed1: Kleisli[Id, Cat, String] = greetKitty.flatMap(greet => feedKitty.map(feed => s"$greet. $feed"))
  val greetAndFeed1: Reader[Cat, String] = greetKitty.flatMap(greet => feedKitty.map(feed => s"$greet. $feed"))

  val greetAndFeed2: Reader[Cat, String] = for {
    greet <- greetKitty
    feed <- feedKitty
  } yield s"$greet . $feed"

  val r4: Id[String] = greetAndFeed1(Cat("Garfield", "lasagne"))


}
