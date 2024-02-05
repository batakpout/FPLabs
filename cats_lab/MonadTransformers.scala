package cats_labs

import scala.concurrent.Future
import scala.util.Try

/**
  MT are lightweight wrappers for effectful types , used to reduce boilerplate and verbosity, improve readibility.

  Either can be used for error handling in most situations. However, when Either is placed into effectful types
  such as Option or Future, a large amount of boilerplate is required to handle errors.
  Clearly, the updated code is less readable and more verbose: the details of the program are now mixed with the
  error handling. In addition, as more Eithers and Futures are included, the amount of boilerplate required
  to properly handle the errors will increase dramatically.
   EitherT to rescue
  EitherT[F[_], A, B] is a lightweight wrapper for F[Either[A, B]] that makes it easy to compose Eithers and Fs together.


 */
object MonadTrans1 extends App {

  import scala.util.Try
  import cats.syntax.all._

  def parseDouble(s: String): Either[String, Double] =
    Try(s.toDouble).map(Right(_)).getOrElse(Left(s"$s is not a number"))

  def divide(a: Double, b: Double): Either[String, Double] =
    Either.cond(b != 0, a / b, "Cannot divide by zero")

  def divisionProgram(inputA: String, inputB: String): Either[String, Double] =
    for {
      a <- parseDouble(inputA)
      b <- parseDouble(inputB)
      result <- divide(a, b)
    } yield result

  println(divisionProgram("4", "2"))
  println(divisionProgram("a", "b"))
}

object MonadTrans2 extends App {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future

  def parseDouble(s: String): Either[String, Double] =
    Try(s.toDouble).map(Right(_)).getOrElse(Left(s"$s is not a number"))

  def divide(a: Double, b: Double): Either[String, Double] =
    Either.cond(b != 0, a / b, "Cannot divide by zero")

  def parseDoubleAsync(s: String): Future[Either[String, Double]] =
    Future.successful(parseDouble(s))

  def divideAsync(a: Double, b: Double): Future[Either[String, Double]] =
    Future.successful(divide(a, b))

  def divisionProgramAsync(inputA: String, inputB: String): Future[Either[String, Double]] =
    parseDoubleAsync(inputA) flatMap { eitherA =>
      parseDoubleAsync(inputB) flatMap { eitherB =>
        (eitherA, eitherB) match {
          case (Right(a), Right(b)) => divideAsync(a, b)
          case (Left(err), _) => Future.successful(Left(err))
          case (_, Left(err)) => Future.successful(Left(err))
        }
      }
    }

  divisionProgramAsync("4", "a").map {
    case Right(_) => println("pass")
    case Left(_) => println("fail")
  }
}

object MonadTrans3 extends App {

  import cats.data.EitherT
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future

  def parseDouble(s: String): Either[String, Double] =
    Try(s.toDouble).map(Right(_)).getOrElse(Left(s"$s is not a number"))

  def divide(a: Double, b: Double): Either[String, Double] =
    Either.cond(b != 0, a / b, "Cannot divide by zero")

  def parseDoubleAsync(s: String): Future[Either[String, Double]] =
    Future.successful(parseDouble(s))

  def divideAsync(a: Double, b: Double): Future[Either[String, Double]] =
    Future.successful(divide(a, b))

  def divisionProgramAsync(inputA: String, inputB: String): EitherT[Future, String, Double] =
    for {
      a <- EitherT(parseDoubleAsync(inputA))
      b <- EitherT(parseDoubleAsync(inputB))
      result <- EitherT(divideAsync(a, b))
    } yield result

  println(divisionProgramAsync("4", "2").value)
  println(divisionProgramAsync("a", "b").value)
}

/**
   OptionT[F[_], A] is a light wrapper on an F[Option[A]]. Speaking technically, it is a monad transformer for Option
   OptionT can be more convenient to work with than using F[Option[A]] directly.
 */
object OptionT1 extends App {

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  val customGreeting: Future[Option[String]] = Future.successful(Some("welcome back, Lola"))

  val excitedGreeting: Future[Option[String]] = customGreeting.map(_.map(_ + "!"))

  val hasWelcome: Future[Option[String]] = customGreeting.map(_.filter(_.contains("welcome")))

  val noWelcome: Future[Option[String]] = customGreeting.map(_.filterNot(_.contains("welcome")))

  val withFallback: Future[String] = customGreeting.map(_.getOrElse("hello, there!"))

  // need to map over Future or effectful type

  import cats.data.OptionT
  import cats.syntax.all._

  val customGreetingT: OptionT[Future, String] = OptionT(customGreeting)

  val excitedGreeting2: OptionT[Future, String] = customGreetingT.map(_ + "!")

  val withWelcome2: OptionT[Future, String] = customGreetingT.filter(_.contains("welcome"))

  val noWelcome2: OptionT[Future, String] = customGreetingT.filterNot(_.contains("welcome"))

  val withFallback2: Future[String] = customGreetingT.getOrElse("hello, there!")

  customGreetingT.value // give underlying instance of F[Option[A]]
}
