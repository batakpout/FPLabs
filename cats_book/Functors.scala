package cats_book

import cats.Functor

/**
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def m[A, B](f: A) = 10//(g: A => B)= ???

  def r[A](a: A):A => Int = m(_)
}
*/

object Functors extends App {

  val f1: Int => Double = x => x.toDouble
  val f2: Double => Boolean = x => x > 10

  import cats.instances.function._
  import cats.syntax.functor._

  val f3: Int => Boolean = f1 map f2
  val f4: Int => Boolean = f1 andThen f2
  val f5 = f2(f1(1))

  val f6: Int => String = f3.map(_.toString).map(_.trim)

  def myMethod[F[_] : Functor] = {
    val functor = Functor.apply[F]

  }
}

object FunctorLaws extends App {

  import cats.Functor
  import cats.syntax.functor._

  def methods[F[_] : Functor, A](fa: F[A]) = {
    fa.map(a => a) == fa // identity law
    // && fa.map(x => g(f(x))) == fa.map(f).map(g) composition law
  }
}

object CatsFunctor extends App {

  import cats.Functor

  val o = Option(1)
  val f1 = Functor[Option].map(o)(_ + 1)

  import cats.syntax.functor._

  val f2 = o.map(_ + 1)
  println(f1)
  println(f2)

  val f: Int => Boolean = x => x > 10

  val g: Option[Int] => Option[Boolean] = Functor[Option].lift(f)

  val l = List(1, 2, 3, 4)
  val fC = Functor[List].as(l, "Aas")
  println(g(Some(22)))
  println(fC)

  def doMath[F[_], A, B](fa: F[A])(f: A => B)(implicit fu: Functor[F]):F[B] =
    fu.map(fa)(f)

  def doMath2[F[_]: Functor, A, B](fa: F[A])(f: A => B): F[B] =
    fa.map(f)

  doMath[List, Int, Int](List(1,2,3))(_ + 1)
  doMath2[Option, Int, Double](Option(190))(a => a + 1.2)

  object FunctorSyntax {
    implicit class FunctorOps[F[_], A](val src: F[A]) extends AnyVal {
      def map2[B](f: A => B)(implicit fC: Functor[F]):F[B] = fC.map(src)(f)
    }
  }

  //as method as syntax
  List(1,2,3).as("As")

   implicit val optionFunctor: Functor[Option] = new Functor[Option] {
      def map[A, B](fa: Option[A])(f: A=>B):Option[B] =
        fa.map(f)
   }

  import cats.Functor
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext

  implicit def futureFunctor(implicit ex: ExecutionContext): Functor[Future] = new Functor[Future] {
    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }

}

object BinaryTreeFunctor extends App {
    sealed trait Tree[+A]
    final case class Branch[A](valeleft: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]

    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }
    }

  val tree1: Tree[Int] = Branch(Branch(Leaf(33), Leaf(11)), Branch(Leaf(22), Leaf(6)))
  //todo://val tree1: Branch[Int] = Branch(Branch(Leaf(33), Leaf(11)), Branch(Leaf(22), Leaf(6))) CTE, check in Chapter 1 of invariance issue later
  import cats.syntax.functor._
  val tree2 = tree1.map(_ + 2)

  println(tree1)
  println(tree2)
}

