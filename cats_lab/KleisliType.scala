package cats_labs

import cats.data.Kleisli

/**
Kleisli is an effectful function compostion
 Kleisli enables composition of functions that return a monadic value
 Benefits:
 * Allows programming in a more composition like style.
 * Abstracts away the lifting of values into a Monad.
 */
object KelisliType1 extends App {

  // case 1
  val parse: String => Option[Int] =
    s => if (s.matches("-?[0-9]+")) Some(s.toInt) else None

  val reciprocal: Int => Option[Double] =
    i => if (i != 0) Some(12.0 / i) else None



  //FUNCTION COMPOSITION

  /**
   * This is where the problem starts. If you want to compose both functions, you cannot use compose
   * or even do it as a variable like the original ones. Instead, you have to declare a function and
   * start nesting flatMaps, which really harms readability.
   */
  def parseAndReciprocal(s: String): Option[Double] = parse(s).flatMap(reciprocal)

  println(parseAndReciprocal("6"))


  //USING KLEISLI
  // final case class Kleisli[F[_], -A, B](run: A => F[B]) { self =>
  val parseKleisli: Kleisli[Option, String, Int] =
  Kleisli(parse)

  val reciprocalKleisli: Kleisli[Option, Int, Double] =
    Kleisli(reciprocal)


  val x1 = reciprocalKleisli(2)
  println(x1)
  val parseAndReciprocalKleisli: Kleisli[Option, String, Double] = parseKleisli andThen reciprocalKleisli
  val x2: Option[Double] = parseAndReciprocalKleisli("4")
  val x3: String => Option[Double] = parseAndReciprocalKleisli.run
  println(x2)
}
