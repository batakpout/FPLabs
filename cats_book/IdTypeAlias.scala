package cats_book

import cats.{Id, Monad}

object IdMonad extends App {
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  def sumSquareFC[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = {
    for {
      x <- a
      y <- b
    } yield (x * x) + (y * y)
  }

  //sumSquareFC(1, 2) //CTE
    sumSquareFC(1: Id[Int], 2: Id[Int])
    sumSquareFC(Option(1), Option(2))
    sumSquareFC(List(1), List(2))

  /**
    type Id[A] = A
   */

  val x1: Id[String] = "Dave" : Id[String]
  val x2: Id[Int] = 123 : Id[Int]
  val x3: Id[List[Int]] = List(1,2,3) : Id[List[Int]]

  val m1: Id[Int] = Monad[Id].pure(1)
  val m2: Id[Int] = Monad[Id].flatMap(m1)(_ + 2)
   println(m1)
   println(m2)

  val m3: Id[Int] = for {
    x <- m1
    y <- m2
  } yield x + y
  println(m3)
}

object CreateMapFlatMapPureForId extends App {
  
    import cats.Id
    def pure[A](a: A): Id[A] = a
    def map[A, B](initial: Id[A])(f: A => B): Id[B] = f(initial)
    def flatMap[A, B](initial: Id[A])(f: A => Id[B]): Id[B] = f(initial)
    
    pure(1)
    map(1)(_ + 2)
    flatMap(1)(_ + 2)
}
