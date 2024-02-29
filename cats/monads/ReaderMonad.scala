package cats.monads
import cats.data.Reader
import cats.Id
/**
   - Reader is a monad that allows us to sequence operations that depend on some input.
   - Instances of Readers wrap up f's on 1-arg, providing us with useful methods for composing them.
   - one common use of RM is DI (dependency-injection)
   - if we have bunch of ops that depends on external conf. we can chain them using Reader to produce
     one large ops that accepts that conf. as parameter and runs our program in specified order.
   - Reader is implemented in terms of Kleisli
 */
object ReaderMonad1 extends App {
  final case class Cat(name: String, favoriteFood: String)
  val catName: Reader[Cat, String] = Reader(cat => cat.name) /** create a Reader[A,B] from f A => B*/

  val f1: Cat => Id[String] = catName.run
  val r1 = f1(Cat("Garfield", "lasagne"))
  val r2 = catName(Cat("Garfield", "lasagne"))
  println(r1)
  println(r2)
}

object ReaderMonadCompose extends App {
  final case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] = Reader(cat => cat.name)
  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")
  val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed1: Reader[Cat, String] = greetKitty.flatMap(greet => feedKitty.map(feed => s"$greet. $feed"))
  val greetAndFeed2: Reader[Cat, String] = for {
    greet <- greetKitty
    feed <- feedKitty
  } yield s"$greet . $feed"

  val r1: Id[String] = greetAndFeed1(Cat("Garfield", "lasagne"))
  println(r1)
}

/**
  Classic use of Reader is to build a program that accepts a conf. as a parameter
 */

object LoginSystem extends App {

  final case class Db(usernames: Map[Int, String], passwords: Map[String, String])

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
