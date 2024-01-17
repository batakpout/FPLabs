package cats_book

object EitherMonad1 extends App {

  //In addition to creating instances of Right, Left directly
  // we can also import asRight and asLeft extension methods from cats.syntax.either
  val either1: Right[Nothing, Int] = Right(10)
  val either2: Either[String, Int] = Right(30)
  //val either2: Right[  , Int] = Right(30) ; actual type inferred by compiler
  //import cats.syntax.either._
  val r: Either[String, Int] = for {
    a <- either1
    b <- either2
  } yield a + b
  println(r) // Right(40)

  import cats.syntax.either._

  val a1: Either[String, Int] = 3.asRight[String]
  val b1 = 4.asRight[String]

  val r1: Right[Nothing, Int] = Right(0)
  val r2: Either[String, Int] = r1.map(_ + 1)
 /* def countPositive(nums: List[Int]) =
    nums.foldLeft(Right(0)){ (acc, num) =>
      if(num > 0) acc.map(_ + 1)
      else Left("Negative Stopping!")
    }*/
  // they help avoid type inference problems caused by over-narrowing
 //override def foldLeft[B](z: B)(op: (B, A) => B): B = {
  def countPositive(nums: List[Int]) =
    nums.foldLeft(0.asRight[String]) { (acc, num) =>
      if (num > 0) acc.map(_ + 1)
      else Left("Negative Stopping!")
    }

   println(countPositive(List(1,2,3,4,5)))
   println(countPositive(List(1,-2)))
}

object EitherMonad2 extends App {

  //cat.syntax.either add some useful extension methods to Either companion object
   import cats.syntax.either._
  val e1: Either[NumberFormatException, Int] = Either.catchOnly[NumberFormatException]("foo".toInt)
  val e2: Either[NumberFormatException, Int] = Either.catchOnly[NumberFormatException]("12".toInt)

  val e3: Either[Throwable, Nothing] = Either.catchNonFatal(sys.error("Badness"))
  val e4: Either[Throwable, Int] = Either.catchNonFatal(10)

  // create either from other data types
  val e5: Either[Throwable, Int] = Either.fromTry(scala.util.Try("foo".toInt))
  val e6: Either[Throwable, Int] = Either.fromTry(scala.util.Try("12".toInt))

  val e7: Either[String, Int] = Either.fromOption[String, Int](None, "Badness")
  val e8: Either[String, Int] = Either.fromOption[String, Int](Some(1), "Badness")

  println(e1)
  println(e2)
  println(e3)
  println(e4)
  println(e5)
  println(e6)
  println(e7)
  println(e8)

  println("--------")

  //transforming eithers: cats.syntax.either also add some useful methods for instances of Either

  val e9: Int = "Error".asLeft[Int].getOrElse(12) // 12
  val e10: Int = 10.asRight[String].getOrElse(22) // 10

  val e11: Either[String, Int] = "Error".asLeft[Int].orElse(2.asRight)

  val e12 = -1.asRight[String].ensure("Must be non-negative!")(_ > 0)
  val e13 = 21.asRight[String].ensure("Must be non-negative!")(_ > 0)

  println(e9)
  println(e10)
  println(e11)
  println(e12)
  println(e13)
  println("$$$$$$$$$")
  //recover and recoverWith provide similar error handling to their namesakes on Future

  val e14: Either[String, Int] = "error".asLeft[Int].recover {
    case _: String => -1
  }

  val e15: Either[String, Int] = "error".asLeft[Int].recoverWith {
    case _: String => Right(1)
  }

  println(e14)
  println(e15)

  println("###############")

  val e16: Either[String, Int] = 1.asRight[String].leftMap(_.reverse)
  val e17: Either[String, Int] = "oof".asLeft[Int].leftMap(_.reverse)

  val e18: Either[String, Int] = 1.asRight[String].bimap(_.reverse, _ * 6)
  val e19: Either[String, Int] = "oof".asLeft[Int].bimap(_.reverse, _ * 6)

  println(e16)
  println(e17)
  println(e18)
  println(e19)


}
