package circe_lab

object AutomaticDerivations extends App {

  import io.circe.syntax._
  import io.circe.generic.auto._
  //fully automatic code derivations
 // Circe uses shapeless to automatically derive the necessary type class instances
  case class Person(name: String)
  case class Greeting(salutation: String, person: Person, exclamationMarks: Int)

  println(Greeting("Hey", Person("aamir"), 3).asJson)
}