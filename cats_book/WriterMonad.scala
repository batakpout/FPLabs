package cats_book

import cats.Id
import cats.data.WriterT

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
  val r3: (Vector[String], Int) = writer.run
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
