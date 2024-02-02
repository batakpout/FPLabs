object Stream1 extends App {

  import cats.effect.IO
  import fs2.Stream

  val eff: Stream[IO, Int] = Stream.eval(IO {
    println("BEING RUN!!");
    1 + 1
  })
  val r: IO[List[Int]] = eff.compile.toList
  implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global

  r.unsafeRunSync
}


object Converter extends IOApp.Simple {

  import cats.effect.{IO, IOApp}
  import fs2.{Stream, text}
  import fs2.io.file.{Files, Path}

  val converter: Stream[IO, Unit] = {
    def fahrenheitToCelsius(f: Double): Double =
      (f - 32.0) * (5.0 / 9.0)

    Files[IO].readUtf8Lines(Path("fahrenheit.txt"))
      .filter(s => s.trim.nonEmpty && !s.startsWith("//"))
      .map(line => fahrenheitToCelsius(line.toDouble).toString)
      .intersperse("\n")
      .through(text.utf8.encode)
      .through(Files[IO].writeAll(Path("celsius.txt")))
  }

  def run: IO[Unit] =
    converter.compile.drain
}

import cats.effect.{IO, IOApp}
import fs2.Stream

object ParallelFs2 extends IOApp.Simple {

  // Define a large Scala list as a source
  val largeList: List[Int] = (1 to 10000).toList

  // Define the parallelism factor
  val parallelism: Int = 4

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
    .parEvalMapUnordered(parallelism)(processElement) //Like #evalMap, but will evaluate effects in parallel, emitting the results downstream.

  // Print the results
  override def run: IO[Unit] =
    resultStream
      .compile
      .toList
      .flatMap(results => IO(results.foreach(println)))
}

object fs2Example extends App {

  import fs2.Stream

  val s0 = Stream.empty
  // s0: Stream[fs2.package.Pure, Nothing] = Stream(..)
  val s1: Stream[Pure, Int] = Stream.emit(1)
  // s1: Stream[[x]fs2.package.Pure[x], Int] = Stream(..)
  val s1a = Stream(1, 2, 3) // variadic
  // s1a: Stream[[x]fs2.package.Pure[x], Int] = Stream(..)
  val s1b: Stream[Pure, Int] = Stream.emits(List(1, 2, 3)) // accepts any Seq
  // s1b: Stream[[x]fs2.package.Pure[x], Int] = Stream(..)
  /** The s1 stream has the type Stream[Pure, Int].Its output type is of course Int, and its effect type is
   * Pure, which means it does not require evaluation of any effects to produce its output.
   * Streams that don't use any effects are called pure streams.
   * You can convert a pure stream to a List or Vector using: */

  println(s1.toList)
}

