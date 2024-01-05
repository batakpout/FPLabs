package pure_fp

import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
/**
 * ->
 */
object WhyFutureIsNTRT1 extends App {
  /**
    -> Problem is creating a Future that induces a side-effect is in itself a side-effect, due to Future's eager nature
       This break referential transparency i.e if you create a Future that only prints to the console, the future
       will be run immediately and run the side-effect without you asking it to.
       e.g: snippet below:
   */

  val x: Future[Unit] = for {
    _ <- Future {
      println("Foo")
    }
    _ <- Future {
      println("Foo")
    }
  } yield ()

  Await.result(x, 2.seconds)

  /**
   -> This results in "Foo" being printed twice. Now if Future was referentially transparent we should be able
      to get the same result in the non-inlined version below: snippet below
   */
    println("-------")
  val printFuture = Future {
    println("Foo")
  }

  val y: Future[Unit] = for {
    _ <- printFuture
    _ <- printFuture
  } yield ()

  Await.result(y, 2.seconds)

  /**
    -> However, this instead prints "Foo" only once and even more problematic,
      it prints it no matter if you include the for-expression or not.
    -> With referential transparency expression we should be able to inline any expression without changing the
       semantics of the program.Future can not guarantee this, therefore it break RT and is inherently effectful.
   */

}

/**
   we wrap the side-effect computation in e.g IO and therefore program to a pure interface whose implementation
   nevertheless relies on effects at the end of the day. But since our API remains pure, these effects aren't
   side effects.
 */

//todo: try the same with IO and ensure that IO is pure
