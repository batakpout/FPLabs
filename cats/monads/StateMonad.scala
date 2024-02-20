package cats.monads

/**
 * . An instance of State is a function that does two things:
 * 1. transform an input state to output state
 * 2. computes a result
 * . instances of State represent f's of type S => (S, A);   State and Result
 * .
 */

import cats.Eval
import cats.data.{IndexedStateT, State}

object State1 extends App {

  val a = State[Int, String] { s => (s + 2, s"Result") }
  //IndexedStateT[F[_], SA, SB, A](val runF: F[SA => F[(SB, A)]])
  //in case of State, F is Eval for stack-safety
  val evalResult: Eval[(Int, String)] = a.run(10)
  val (state, result) = a.run(10).value
  val justTheState = a.runS(10).value
  val justTheResult = a.runA(10).value

  println(s"state and result: ($state, $result)")
  println(s"justTheState: $justTheState")
  println(s"justTheResult: $justTheResult")

}

/**
  . like Reader, Writer, power of State monad comes from combining instances
  . map and flatMap threads state from one instance to another
  . each individual instance represents an atomic state transformation, and their combinations
    represents a complete sequence of changes
  . This threading of state through the computations is what allows us to model stateful computations in a
    functional and composable way using the State monad or in this way we can model mutable state in a pure
    functional way, without using actual mutation.
 */


object State2 extends App {

 val step1 = State[Int, String] { num =>
   val ans = num + 1
   (ans, s"Result of step1: $ans")
 }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step1: $ans")
  }

  val both1: IndexedStateT[Eval, Int, Int, (String, String)] = step1.flatMap { a => step2.map{ b => (a, b)}}
  val both2: State[Int, (String, String)] = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val (state, result) = both1.run(20).value
  println(s"state and result: ($state, $result)")
}
