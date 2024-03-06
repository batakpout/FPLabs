package cats.monads

import cats.Monad

/**
 * We can define a Monad for a Custom type by providing impl. of 3 methods:
  pure, flatMap and tailRecM
 */

object CustomMonads1 extends App {

  val optionMonad = new Monad[Option] {
    def pure[A](a: A): Option[A] = Some(a)

    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)

    /**
     * this method is an optimization used in cats to limit the amount of stack space
      consumed by nested calls to flatMap.
     */

    def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None => None
        case Some(Right(b)) => Some(b)
        case Some(Left(a)) => tailRecM(a)(f)
      }

  }

  import cats.syntax.flatMap._

  def retry[F[_] : Monad, A](start: A)(f: A => F[A]): F[A] =
    f(start).flatMap { a =>
      //println(s"a = $a")
      retry(a)(f)
    }

  retry(100)(a => if (a == 0) None else Some(a - 1))
  /** unfortunately it is not stack safe */
  //retry(100000)(a => if (a == 0) None else Some(a - 1)) //kaboom!

  /** with tailRecM it runs with any SO error no matter how many times we recurse */

  import cats.syntax.functor._

  def retryTailRecM[F[_] : Monad, A](start: A)(f: A => F[A]): F[A] =
    Monad[F].tailRecM(start) { a =>
      f(a).map(a2 => Left(a2))
    }

  retryTailRecM(1000000)(a => if (a == 0) None else Some(a - 1))
  /** we can rewrite retry in terms of iterateWhileM and we don't have to explicitly call tailRecM */

    import cats.syntax.monad._
  def retryM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
    start.iterateWhileM(f)(_ => true)
  retryM(1000000)(a => if (a == 0) None else Some(a - 1))

  /**
     - All of build-in monads in Cats have tail-recursive implementation of tailRecM,
       although writing one for custom monads can be challenge
    */
}

object CustomIdentityMonad extends App {

  type Identity[T] = T

  val identityMonad = new Monad[Identity] {
    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(a) => tailRecM(a)(f)
      case Right(b) => b
    }

    override def pure[A](x: A): Identity[A] = x
  }
}
