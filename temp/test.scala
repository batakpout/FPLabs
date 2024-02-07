package temp

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO, IOApp, Resource}
import eu.timepit.refined.types.all.NonNegLong

object MyApp extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val io: IO[Int] = IO {
      // Some effectful computation
      Thread.sleep(1000)
      42
    }

    io.flatMap(result => IO(println(s"Result: $result"))).as(ExitCode.Success)
  }
}

object IOTutorial extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {

    val io1: IO[Unit] = for {
      _ <- IO {
        println("Foo1")
      }
      _ <- IO {
        println("Foo2")
      }
    } yield ()

    val printIO: IO[Unit] = IO(println("Foo"))

    val program: IO[Unit] = for {
      _ <- printIO
      _ <- printIO
    } yield ()

    implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global
    io1.unsafeRunSync()

    // Print separator
    println("====")

    // Run program.as
    program.as(ExitCode.Success)
  }
}

object FutureParallelTest extends App {

  import cats.implicits._

  import scala.concurrent._
  import scala.concurrent.duration._

  implicit val ec = ExecutionContext.global

  val hello = Future(println(s"[${Thread.currentThread.getName}] Hello"))

  val world = Future(println(s"[${Thread.currentThread.getName}] World"))

  val hw1: Future[Unit] = for {
    _ <- hello
    _ <- world
  } yield ()

  Await.ready(hw1, 5.seconds)
  println("=====")

  val hw2: Future[Unit] =
    (hello, world).mapN((_, _) => ())

  Await.ready(hw2, 5.seconds)

  Thread.sleep(100000)

}

object FutureParallelTestDef extends App {

  import cats.implicits._

  import scala.concurrent._
  import scala.concurrent.duration._

  implicit val ec = ExecutionContext.global

  def hello = Future(println(s"[${Thread.currentThread.getName}] Hello"))

  def world = Future(println(s"[${Thread.currentThread.getName}] World"))

  val hw1: Future[Unit] = for {
    _ <- hello
    _ <- world
  } yield ()

  Await.ready(hw1, 5.seconds)
  println("=====")

  val hw2: Future[Unit] =
    (hello, world).mapN((_, _) => ())

  Await.ready(hw2, 5.seconds)

  Thread.sleep(100000)

}

import cats.implicits._


//execute two IOs in parallel


object TestFuture extends App {

  import scala.concurrent.{ExecutionContext, Future}
  import scala.util.{Failure, Success}

  def retryFuture[A](future: => Future[A], retries: Int)(implicit ec: ExecutionContext): Future[A] = {
    future.flatMap { result =>
      Future.successful(result)
    }.recoverWith {
      case exception if retries > 0 =>
        retryFuture(future, retries - 1)
        Future.failed(new RuntimeException(s"All retries failed. Last exception: $exception"))
    }
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  val exampleFuture: Future[Int] = Future {
    if (scala.util.Random.nextBoolean()) 42 else throw new RuntimeException("API call failed")
  }

  val resultFuture = retryFuture(exampleFuture, 3)

  resultFuture onComplete {
    case Success(value) => println(s"Success! Result: $value")
    case Failure(exception) => println(s"Failed. Exception: $exception")
  }

  Thread.sleep(5000)


}

object WithIO extends App {

  import scala.concurrent.duration._


  def retryIO[A](io: => IO[A], retries: Int): IO[A] = {
    io.handleErrorWith { exception =>
      if (retries > 0) IO.sleep(1.second) *> retryIO(io, retries - 1)
      else IO.raiseError(new RuntimeException(s"All retries failed. Last exception: $exception"))
    }
  }

  implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global

  val exampleIO: IO[Int] = IO {
    if (scala.util.Random.nextBoolean()) 42 else throw new RuntimeException("API call failed")
  }

  val resultIO = retryIO(exampleIO, 3)

  resultIO.attempt.unsafeRunSync() match {
    case Right(value) => println(s"Success! Result: $value")
    case Left(exception) => println(s"Failed. Exception: $exception")
  }

  Thread.sleep(5000)
}


object IOError extends App {
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail"))
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt
}
import cats.effect.{IO, IOApp}
import fs2.Stream
import fs2.io.file.{Files, Path}

object ResourceExample extends IOApp.Simple {

  // Define a simple resource that acquires and releases a file handle
  def fileResource(path: Path): Resource[IO, Unit] =
    Resource.make(IO(println(s"Acquiring file handle for $path")))(_ => IO(println(s"Releasing file handle for $path")))

  // Define a simple resource that acquires and releases a network connection
  def networkResource(host: String, port: Int): Resource[IO, Unit] =
    Resource.make(IO(println(s"Opening connection to $host:$port")))(_ => IO(println(s"Closing connection to $host:$port")))

  override def run: IO[Unit] = {
    val filePath = Path("example.txt")
    val host = "localhost"
    val port = 8080

    val program = for {
      // Acquire and use the file resource
      _ <- fileResource(filePath).use { _ =>
        IO(println("File resource in use"))
      }
      // Acquire and use the network resource
      _ <- networkResource(host, port).use { _ =>
        IO(println("Network resource in use"))
      }
    } yield ()

    program.as(ExitCode.Success)
  }
}

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.Outcome
import scala.concurrent.duration._
object StupidFizzBuzz {
  val runIO: IO[Unit] =
    for {
      ctr <- IO.ref(0)

      wait = IO.sleep(1.second)
      poll = wait *> ctr.get

      _ <- poll.flatMap(IO.println(_)).foreverM.start
      _ <- poll.map(_ % 3 == 0).ifM(IO.println("fizz"), IO.unit).foreverM.start
      _ <- poll.map(_ % 5 == 0).ifM(IO.println("buzz"), IO.unit).foreverM.start

      _ <- (wait *> ctr.update(_ + 1)).foreverM.void
    } yield ()
}

object Main extends IOApp.Simple {

  def run: IO[Unit] =
    StupidFizzBuzz.runIO.guaranteeCase {
      case Outcome.Succeeded(fa) => fa
      case Outcome.Canceled() => IO.println("I was cancelled!")
      case Outcome.Errored(_) => IO.println("I errored!")
    }
}




