/**
Backpressure allows running effects through the rate-limiting strategy.
Backpressure can be utilized in a scenario where a 
large number of tasks need to be processed, but only a certain number of
resources are available. In other words, backpressure is needed
when there is not enough time or resources available for processing 
all the requests on your system.
**/
object BackpressureExample extends IOApp {

  def processElement(value: Int): IO[Unit] =
    IO(println(s"Processing element: $value"))

  override def run(args: List[String]): IO[ExitCode] = {
    val program: IO[Unit] = for {
      // Create a backpressure instance with Lossless strategy and buffer size 1
      backpressure <- Backpressure[IO](Backpressure.Strategy.Lossless, 1)

      // Start two asynchronous tasks with backpressure
      fiber1 <- backpressure.metered(IO.sleep(1.second) *> processElement(1)).start
      fiber2 <- backpressure.metered(IO.sleep(1.second) *> processElement(2)).start
      fiber3 <- backpressure.metered(IO.sleep(1.second) *> processElement(23)).start
      fiber4 <- backpressure.metered(IO.sleep(1.second) *> processElement(8)).start

      // Wait for the tasks to complete
      /**
      While fiber1 is processing, if another element arrives (e.g., fiber2), it is placed in the backpressure buffer since the buffer size is limited to 1.
      The backpressure system allows only one element to be processed at a time, preventing the buffer from overflowing.
      */
      _ <- fiber1.join
      _ <- fiber2.join
      _ <- fiber3.join
      _ <- fiber4.join
    } yield ()

    // Run the program
    program.as(ExitCode.Success)
  }
}
