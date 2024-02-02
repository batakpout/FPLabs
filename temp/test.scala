package temp

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO, IOApp}

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

object IOComposition extends App {

  /**
   * IO doesn’t provide any support for the effect of parallelism! And this is by design,
   * because we want different effects to have different types, as per our Effect Pattern
   */
  implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global

  val hello = IO(println(s"[${Thread.currentThread.getName}] Hello"))
  val world = IO(println(s"[${Thread.currentThread.getName}] World"))
  val hw1: IO[Unit] = for {
    _ <- hello
    _ <- world
  } yield ()

  val hw2: IO[Unit] =
    (hello, world).parMapN((_, _) => ())

  hw1.unsafeRunSync()
  println("====")
  hw2.unsafeRunSync()

  Thread.sleep(5000)
  //unlike Future, IO itself doesn’t provide any support for parallelism.
  //So how can we achieve it?
}

//execute two IOs in parallel

object ParallelIOTutorial extends IOApp {

  val io1: IO[Unit] = IO(println("Executing IO 1"))
  val io2: IO[Unit] = IO(println("Executing IO 2"))

  override def run(args: List[String]): IO[ExitCode] = {
    val parallelIO: IO[Unit] = (io1, io2).parTupled.flatMap {
      case (_, _) =>
        IO(println("Both IOs completed"))
    }

    parallelIO.as(ExitCode.Success)
  }
}

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

object TraverseIO extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {

    val ioList: List[IO[Int]] = List(
      IO(1),
      IO(2),
      IO(3)
    )
    import cats.syntax.all._
    val modifiedIOList: IO[List[Int]] = ioList.traverse(io => io.map(_ + 10))
    modifiedIOList.flatMap(v => IO.println(v)).as(ExitCode.Success)

  }

}

object TraverseOptionList extends App {

  import cats.implicits._


  val optionList: List[Option[Int]] = List(Some(1), Some(2), Some(3))

  val modifiedOptionList: Option[List[Int]] = optionList.traverse(opt => opt.map(_ + 10))

  modifiedOptionList.foreach(result => println(s"Modified Option List: $result"))

}

object IOError extends App {
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail"))
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

}

import cats.effect.Resource

import java.io.{FileInputStream, FileOutputStream}

object ResourceExample extends App {

  // Dummy class representing the content of a file
  case class FileContent(content: String)

  // Function to open a file
  def openFile(name: String): Resource[IO, FileInputStream] =
    Resource.make(IO(new FileInputStream(name)))(stream => IO(stream.close()).handleErrorWith(_ => IO.unit))

  // Function to close a file
  def close(file: FileInputStream): IO[Unit] =
    IO(file.close()).handleErrorWith(_ => IO.unit)

  // Function to read from a file
  def read(file: FileInputStream): IO[FileContent] =
    IO {
      val byteArray = new Array[Byte](file.available())
      file.read(byteArray)
      FileContent(new String(byteArray))
    }.handleErrorWith(_ => IO(FileContent("")))

  // Function to write to a file
  def write(file: FileOutputStream, content: String): IO[Unit] =
    IO(file.write(content.getBytes)).handleErrorWith(_ => IO.unit)

  // Example usage
  val concat: IO[Unit] =
    (
      for {
        in1 <- openFile("file1")
        in2 <- openFile("file2")
        out <- openFile("file3")
      } yield (in1, in2, out)
      ).use { case (file1, file2, file3) =>
      for {
        content1 <- read(file1)
        content2 <- read(file2)
        _ <- write(new FileOutputStream("result.txt"), content1.content + content2.content)
      } yield ()
    }

  implicit val runtime = cats.effect.unsafe.IORuntime.global
  concat.unsafeRunSync()
}


