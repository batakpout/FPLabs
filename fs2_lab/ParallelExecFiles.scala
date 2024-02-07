package fs2_lab

import cats.effect.{IO, IOApp}
import fs2.{Pure, Stream}

object ParallelFileFs2 extends IOApp.Simple {

  // Define a large Scala list as a source
  val largeList: List[Int] = (1 to 100).toList

  // Define the parallelism factor
  val parallelism: Int = 8

  // Function to process a single element
  def processElement(i: Int): IO[String] =
    IO {
      // Simulate some processing time
      Thread.sleep(10)
      s"Processed: $i"
    }

  // Create a parallel stream that reads from the list, processes in parallel, and collects the results
  val resultStream: Stream[IO, String] = Stream
    .emits(largeList)
    .covary[IO] //Lifts this stream to the specified effect type.
    .parEvalMapUnordered(parallelism)(processElement) //Like #evalMap, but will evaluate effects in parallel,
  // emitting the results downstream.

  // Print the results
  override def run: IO[Unit] =
    resultStream
      .compile
      .toList
      .flatMap(results => IO(results.foreach(println)))
}


object ParallelRun extends IOApp.Simple {

  import cats.effect.IO
  import cats.effect.IOApp
  import fs2.Stream

  def increaseCounter(counter: Int): IO[Int] = IO.delay {
    println(s"Increasing counter: $counter")
    counter + 1
  }

  override def run: IO[Unit] = {
    val initialCounter = 0
    val numIncrements = 10
    val parallelism = 3

    val stream = Stream
      .emits(1 to numIncrements)
      .covary[IO]
      .parEvalMap(parallelism)(_ => increaseCounter(initialCounter))

    stream.compile.drain
  }

}



object ProcessFileLineByLine extends IOApp.Simple {

  import fs2.io.file.{Files, Path}

  // Function to process each line (in this case, just convert to uppercase)
  def processLine(line: String): String = line.toUpperCase

  // Function to read lines from input file, process each line, and write processed lines to output file
  def processFile(inputPath: Path, outputPath: Path): Stream[IO, Unit] =
    Files[IO]
      .readAll(inputPath) // reads bytes from file
      .through(fs2.text.utf8.decode)// This converts the stream of bytes into a stream of characters using UTF-8 decoding. It assumes that the input file contains UTF-8 encoded text
      .through(fs2.text.lines) //This splits the stream of characters into individual lines
      .map(processLine) //This applies the processLine function to each line of the input file.
      .intersperse("\n")
      .through(fs2.text.utf8.encode) //This converts the stream of processed lines (strings) back into a stream of bytes using UTF-8 encoding.
      .through(Files[IO].writeAll(outputPath))//his writes the stream of bytes to the specified output file

  override def run: IO[Unit] = {
    val inputFilePath = Path("/Users/awani/Downloads/input.txt")
    val outputFilePath = Path("/Users/awani/Downloads/output.txt")

    processFile(inputFilePath, outputFilePath).compile.drain

    /**
    These transformations compose a stream processing pipeline. Each step takes the output of the previous step,
      processes it in some way, and passes it on to the next step. This allows for efficient and scalable processing of
      large files, as the data is processed incrementally as it is read from the input file
     */
  }
}

object Converter extends IOApp.Simple {

  import cats.effect.IO
  import fs2.io.file.{Files, Path}
  import fs2.{Stream, text}

  val converter: Stream[IO, Unit] = {
    def fahrenheitToCelsius(f: Double): Double =
      (f - 32.0) * (5.0 / 9.0)

    Files[IO].readUtf8Lines(Path("/Users/awani/Downloads/fahrenheit.txt"))
      .filter(s => s.trim.nonEmpty && !s.startsWith("//"))
      .map(line => fahrenheitToCelsius(line.toDouble).toString)
      .intersperse("\n")
      .through(text.utf8.encode)
      .through(Files[IO].writeAll(Path("/Users/awani/Downloads/celsius.txt")))
  }

  def run: IO[Unit] =
    converter.compile.drain
}


