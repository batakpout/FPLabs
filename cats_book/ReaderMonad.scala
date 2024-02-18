//> using dep "org.typelevel::cats-core:2.7.0"
package cats_book

import cats.Id

object ReaderMonad1 {

  final case class Cat(name: String, favoriteFood: String)

  import cats.data.Reader

  val catName: Reader[Cat, String] = Reader(cat => cat.name)

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

object ReaderMonadExercise extends App {

  final case class Db(
                       usernames: Map[Int, String],
                       passwords: Map[String, String]
                     )

  import cats.data.Reader

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))


  import cats.syntax.applicative._

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      uNOpt <- findUsername(userId)
      result <- uNOpt match {
        case Some(u) => checkPassword(u, password)
        case None => false.pure[DbReader]
      }
    } yield result
  }

  val users = Map(
    1 -> "john",
    2 -> "kate",
    3 -> "margo"
  )
  val passwords = Map(
    "john" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println {
    checkLogin(1, "zerocool").run(db)
  }
  println {
    checkLogin(1, "hero").run(db)
  }

}

