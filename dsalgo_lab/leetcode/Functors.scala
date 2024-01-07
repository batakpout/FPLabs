package cats_book

import cats.Functor

object Functors extends App {

  val f1: Int => Double = x => x.toDouble
  val f2: Double => Boolean = x => x > 10

  import cats.instances.function._
  import cats.syntax.functor._
  val f3: Int => Boolean = f1 map f2
  val f4: Int => Boolean = f1 andThen f2
  val f5 = f2(f1(1))

   val f6: Int => String = f3.map(_.toString).map(_.trim)

   def myMethod[F[_]: Functor] = {
     val functor = Functor.apply[F]

   }
}

object FunctorLaws extends App {

}
