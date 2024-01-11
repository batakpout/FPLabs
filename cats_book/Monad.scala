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
object Monads extends App {

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




}
