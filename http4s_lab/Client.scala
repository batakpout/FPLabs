package http4s

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.http4s.client.Client
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.client.JavaNetClientBuilder
import cats.syntax.all._


object Client extends App {

  import org.http4s.client.JavaNetClientBuilder

  val httpClient: Client[IO] = JavaNetClientBuilder[IO].create

  import cats.effect.IO
  import cats.syntax.all._
  import org.http4s.implicits.http4sLiteralsSyntax


  def hello(name: String): IO[String] = {
    val target = uri"http://localhost:8080/hello/" / name
    httpClient.expect[String](target)
  }

  val inputs = List("Ember", "http4s", "Scala")

  val getGreetings: IO[List[String]] =
    inputs.parTraverse(hello)

  val helloEmber: IO[String] =
    httpClient.expect[String]("http://localhost:8080/hello/ember")


  println {
    helloEmber.unsafeRunSync()
  }

}

import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.dsl.io._
import cats.effect.IO
import cats.syntax.all._

object printio {
  implicit class DebugHelper[A](ioa: IO[A]) {
    def print(i: Int): IO[A] =
      for {
        a <- ioa
        tn = Thread.currentThread().getName
        _ = println(s"index = $i, Thread-[$tn] , response = $a")
        _ = println(s"---------------------------------------")
      } yield a
  }
}

object printio2 {
  implicit class DebugHelper[A](ioa: IO[A]) {
    def print(index: Int): IO[Any] = {
      ioa.attempt.flatMap {
        case Left(_) =>
          IO(println(s"==Stopped IO at: $index"))
        case Right(_) =>
          IO(println("==Passed IO"))
      }
    }
  }
}







object ClientTest extends App {

  val httpClient: Client[IO] = JavaNetClientBuilder[IO].create


  def httpBinGet(index: Int): IO[String] = {
    val target = uri"http://localhost:8080/hello/" / index
    httpClient.expect[String](target)
  }

  import printio._
  val responses: IO[List[String]] = (1 to 10).toList.parTraverse(index => httpBinGet(index).print(index))

  responses.unsafeRunSync()

  Thread.sleep(6000)

}


object ClientTestCancelled extends App {

  val httpClient: Client[IO] = JavaNetClientBuilder[IO].create

  def httpBinGet(index: Int): IO[String] = {
    val target = Uri.unsafeFromString(s"http://localhost:8080/hello/$index")
    httpClient.expect[String](target)
  }

  import printio2._
 import cats.syntax.all._

  val responses = (1 to 10).toList.parTraverse(index => {
     if (index == 3) {
      IO.raiseError[String](new RuntimeException("IO canceled or failed"))
      //IO.canceled
    } else {
      httpBinGet(index).onCancel(IO.println(s"cancelled IO with index: $index"))
    }
  })

/*  v//al resultIO = responses.map(_.collect { case Left(ex) => ex.getMessage })

  resultIO.flatMap {
    case Nil =>
      IO(println("All IOs completed successfully."))
    case canceledMessages =>
      canceledMessages.traverse(msg => IO(println(s"Error occurred: $msg")))
  }.unsafeRunSync()*/

  // Run the resultIO
  responses.unsafeRunSync()

  Thread.sleep(6000)
}


