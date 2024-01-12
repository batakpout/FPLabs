package cats_book
object MonadExample1 extends App {

  def parseInt(a: String): Option[Int] =
    a.toIntOption

  def divide(a: Int, b: Int): Option[Int] =
    if(b == 0) None else Some(a / b)

  def stringDivideBy(s1: String, s2: String): Option[Int] =
    parseInt(s1).flatMap { i1 =>
      parseInt(s2).flatMap { i2 =>
        divide(i1, i2)
      }
    }

  println(stringDivideBy("10", "2"))
  println(stringDivideBy("10", "aa"))

  def stringDivideByFC(s1: String, s2: String): Option[Int] =
    for {
      i1 <- parseInt(s1)
      i2 <- parseInt(s2)
      r <- divide(i1, i2)
    } yield r
}
object Monads1 extends App {

   trait Monad_[F[_]] {

     def pure[A](a: A): F[A]
     def flatMap[A, B](fa: F[A])(f: A => F[B]):F[B]
     def map[A, B](fa: F[A])(f: A => B):F[B] =
       flatMap(fa)(a => pure(f(a)))

     def ap[A, B](f: F[A => B])(fa: F[A]): F[B]

     //def map2[A, B](fa: F[A])(f: A => B) = {
      // val f = ap(pure(f)) -- to be continued check ap method impl. in Apply , applicative, FlatMap
     }

     /**
       Laws:
       1. left identity: pure(a).flatMap(f) == f(a)
       2. right identity: m.flatMap(pure) == m
       3. associativity: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
      */

      /**
        map method of Option:
        final def map[B](f: A => B): Option[B] =
                         if (isEmpty) None else Some(f(this.get))

        def flatMap[B](f: A => Option[B]): Option[B] =
          if(isEmpty) None else f(this.get)

       */
   }

object Monads2 extends App {
   import cats.Monad
   import cats.instances.option._
   import cats.instances.list._
   val optionMonad: Option[Int] = Monad[Option].pure(1)
   val opt: Option[Int] = Monad[Option].flatMap(optionMonad)(a => Some(a + 2))
   println(opt)

   val listMonad: List[Int] = Monad[List].pure(3)
   val list: List[Int] = Monad[List].flatMap(listMonad)(a => List(a, a * 10))
   val vect: Vector[Int] = Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10))
   println(list)
   println(vect)

   import scala.concurrent._
   import scala.concurrent.duration._
   import scala.concurrent.ExecutionContext.Implicits.global
   Future.successful(1).map(_ + 10) // EC as implicit param to map
   import cats.instances.future._
   val futureMonadInstance: Monad[Future] = Monad[Future]
   val futureMonad = Monad[Future].pure(2)
   val future1 = Monad[Future].flatMap(futureMonad)(x => Future.successful(x + 2))
   val future2 = futureMonadInstance.flatMap(futureMonadInstance.pure(2))(x => futureMonadInstance.pure(x + 2))

   val futureResult1 = Await.result(future1, 1.seconds)
   val futureResult2 = Await.result(future2, 1.seconds)
   println(futureResult1)
   println(futureResult2)
}

object CatsSyntax extends App {

   import cats.syntax.applicative._
   import cats.instances.option._

  val monadI: Option[Int] = 1.pure[Option]

   monadI.flatMap(x => Some(x + 1)) // call flatMap of scala standard lib

   import cats.Monad

   import cats.syntax.flatMap._
   import cats.syntax.functor._

   def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
     a.flatMap(x=> b.map( y => x * x  + y * y))
  import cats.instances.list._
  import cats.instances.option._
  //todo : think about tranformation oriternation here?
   println(sumSquare(List(1,2), List(3,4)))
   println(sumSquare(Option(1), Option(2)))

   def sumSquareFC[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = {
     for {
       x <- a
       y <- b
     } yield (x * x) + (y * y)
   }

  println(sumSquareFC(List(1, 2), List(3, 4)))
  println(sumSquareFC(Option(1), Option(2)))
}
