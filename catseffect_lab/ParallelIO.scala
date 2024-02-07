object ParallelIOs extends App {
  val jobOne = IO {
    10
  }

  val jobTwo = IO {
    10
  }

  //jobOne.flatMap(i => jobTwo.map(s => (i,s))) sequential execution
  val result1: IO[(Int, Int)] = (jobOne, jobTwo).tupled //sequential execution

  //concurrent execution (manually)
  val result2 = for {
    f1 <- jobOne.start
    f2 <- jobTwo.start
    i <- f1.join
    j <- f2.join
  } yield (i, j)

  // concurrent execution (higher level)
  val result3 = (jobOne, jobTwo).parTupled
}

object ParallelIOTutorial extends IOApp {

  object D {
    implicit class DebugHelper[A](ioa: IO[A]) {
      def debugger: IO[A] =
        for {
          a <- ioa
          tn = Thread.currentThread().getName
          _ = println(s"[$tn] $a")
        } yield a
    }
  }

  import D._

  val io1: IO[Unit] = IO(println("Executing IO 1")).debugger
  val io2: IO[Unit] = IO(println("Executing IO 2")).debugger

  override def run(args: List[String]): IO[ExitCode] = {
    val parallelIO: IO[Unit] = (io1, io2).parTupled.flatMap {
      case (_, _) =>
        IO(println("Both IOs completed"))
    }

    parallelIO.as(ExitCode.Success)
  }
}


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
