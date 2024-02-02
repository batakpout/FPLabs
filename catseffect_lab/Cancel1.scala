package catseffect.cancellation

import cats.effect.{IO, IOApp}
import cats.syntax.all._

import scala.concurrent.duration.DurationInt

object CancelProgram1 extends IOApp.Simple {

  import catseffect.debug._

  val chainOfIOs = IO("waiting").debug  >> IO(42) >>= IO.println
  val chainOfIOsWithCancel = IO("waiting").debug >> IO.canceled >> IO(42) >>= IO.println

  val specialPaymentSys: IO[Unit] = (IO("Payment running").flatMap(IO.println) >>
    IO.sleep(1.second) >>
    IO("payment completed").flatMap(IO.println)).onCancel(IO("Mega cancel of doom").flatMap(IO.println).void)

  val cancellationOfDoom = for {
    fib <- specialPaymentSys.start
    _<- IO.sleep(1000.millis) >> fib.cancel
    _<- fib.join
  } yield ()

  val atomicPayment: IO[Unit] = IO.uncancelable(_ => specialPaymentSys) // masking

  val noCancelOfDoom = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(10.millis) >> IO("Attempting cancellation").flatMap(IO.println) >> fib.cancel
    _ <- fib.join
  } yield ()

  import print._
  val inputPass: IO[String] = IO("Input passwd").print >> IO("Typing passwd").print >>
    IO.sleep(2.second) >> IO("MyPassw0rd!").print
  val verifyPass: String => IO[Boolean] = (pw: String) => IO("Verifying...").print >> IO.sleep(4.second) >> IO(pw == "MyPassw0rd!").print

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPass).onCancel(IO("Auth time out. Try again later").print.void)
      verified <- verifyPass(pw).onCancel(IO("Auth time out. Try again later 2").print.void)
      _ <- if (verified) IO("Auth successful").print
      else IO("Auth failed").print
    } yield ()
  }.onCancel(IO("Cancel password").flatMap(IO.println))

  val authProgram: IO[Unit] = for {
    authFib <- authFlow.start
    _ <- IO.sleep(1.seconds) >> IO("Auth time out, attempting canecel").print >> authFib.cancel
    _ <- authFib.join

  } yield ()

  override def run: IO[Unit] = authProgram.void
}
