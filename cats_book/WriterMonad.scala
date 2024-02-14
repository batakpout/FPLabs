package cats_book

import cats.Id
import cats.data.WriterT
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object WriterMonad extends App {

  import cats.data.Writer

  //type Writer[L, W] = WriterT[Id, L, V]
  val writer: WriterT[Id, Vector[String], Int] = Writer(
    Vector("It was the best of times",
      "It was the worst of times"
    ), 1859
  )

  println(writer)

  import cats.syntax.applicative._
  import cats.instances.vector._

  type Logged[A] = Writer[Vector[String], A]
  //val w1 = 123.pure[Writer[Vector[String], Int]]
  val w1 = 123.pure[Logged]

  println(w1)

  import cats.syntax.writer._

  val w2: Writer[Vector[String], Unit] = Vector("log1", "log2").tell

  val w3: Writer[Vector[String], Int] = 123.writer(Vector("log1", "log2"))

  println(w2)
  println(w3)
  println("-----")
  val r1: Id[Int] = writer.value // value from Writer
  println(r1)
  val r2: Id[Vector[String]] = writer.written // log from Writer
  println(r2)
  val r3: Id[(Vector[String], Int)] = writer.run //both value and log from Writer
  println(r3)

}

object WriterMonad2 extends App {

  //final case class WriterT[F[_], L, V](run: F[(L, V)])
  /**
   * def liftF[F[_], L, V](fv: F[V])(implicit monoidL: Monoid[L], F: Applicative[F]): WriterT[F, L, V] =
   * WriterT(F.map(fv)(v => (monoidL.empty, v)))
   */

  val writer: WriterT[Option, List[String], Int] = WriterT.liftF[Option, List[String], Int](Some(123))

  val w1: WriterT[Option, List[String], Int] = WriterT(Some((List("error"), 123)))

  val w2: Option[Int] = w1.value
  val w3: Option[List[String]] = w1.written
  val w4: Option[(List[String], Int)] = w1.run

  val w = WriterT.liftF[Option, List[String], Int](Some(123))
  println(w.tell(List("a", "b", "c")).tell(List("d", "e", "f")).run)
  println(w.tell(List("a", "b", "c")).tell(List("d", "e", "f")).written)

}

object WriterMonad3 extends App {

  type Logged[A] = WriterT[Id, Vector[String], A]

  import cats.syntax.applicative._
  import cats.instances.vector._
  import cats.syntax.writer._

  val w1: WriterT[Id, Vector[String], Int] = 10.pure[Logged].flatMap { x =>
    Vector("a", "b").tell.flatMap { _ =>
      32.writer(Vector("c", "d")).map { y =>
        x + y
      }
    }
  }

  val wx = (12).writer(Vector("a", "b").tell).flatMap { x =>
    (x + 2).writer(Vector("c", "d").tell)
  }

  val w2 = for {
    x <- 10.pure[Logged]
    _ <- Vector("a", "b").tell
    y <- 32.writer(Vector("c", "d"))
  } yield x + y
  println(w1.run)

  import cats.syntax.show._

  val wr1: WriterT[Option, String, Int] = WriterT.liftF[Option, String, Int](Some(123)).tell("error")
  val func: Int => WriterT[Option, String, Int] =
    (i: Int) => WriterT.liftF[Option, String, Int](Some(i * 2)).tell(i.show)
  val wr2 = wr1.flatMap(func)
  println(wr2.run)

  val w3: WriterT[Id, Vector[String], Int] = w1.mapWritten(_.map(_.toUpperCase))
  println("----")
  println(w3)

  val w4 = w1.bimap(
    log => log.map(_ + "%"),
    res => res * 100
  )

  val w5 = w1.mapBoth { (log, res) =>
    val l = log.map(_ + "!")
    val r = res * 100
    (l, r)
  }
  println("******")
  println(w4)
  println(w5)

  println("$$$$$$$$")
  println(w5.reset)
  println(w5.swap)
}

object WriterMonadExercise1 extends App {

  def slowly[A](body: => A) = try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  //factorial(5)


  val fS = Future.sequence(
    Vector(
      Future(factorial(5)),
      Future(factorial(5))
    )
  )

  Await.result(fS, 5.seconds)

}

object FixInterleavedMessages extends App {

  import cats.data.Writer
  import cats.syntax.applicative._
  import cats.syntax.writer._

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) = try body finally Thread.sleep(100)

  def ff(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(ff(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  def factorial(n: Int): Logged[Int] = {
    if (n == 0) 1.writer(Vector(s"fact $n 1"))
    else slowly(factorial(n - 1).flatMap { ans =>
     (ans * n).writer(Vector(s"fact $n ${ans * n}"))
    })
  }

  //running several factorial in Parallel
  val fS = Future.sequence(
    Vector(
      Future(factorial(5)),
      Future(factorial(5)),
    )
  )

  val x: Vector[Logged[Int]] = Await.result(fS, 5.seconds)
  println(x.map(_.written))

}
