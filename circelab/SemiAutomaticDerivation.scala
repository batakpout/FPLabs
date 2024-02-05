/*
package circe_lab

object SemiAutomaticDerivation extends App {

  import io.circe._
  import io.circe.generic.semiauto._
  import io.circe.syntax._

  case class Foo(a: Int, b: String, c: Boolean)

  implicit val fooDecoder: Decoder[Foo] = deriveDecoder
  implicit val fooEncoder: Encoder[Foo] = deriveEncoder

  val foo = Foo(1, "foo", true)
  println(foo.asJson)

  println("----")
  println(foo.asJson.as[Foo])

}

object SemiAutomaticDerivation2 extends App {

  import io.circe.generic.JsonCodec
  import io.circe.syntax._

  @JsonCodec case class Bar(i: Int, s: String)
  println(Bar(1,"bar").asJson)
  println(Bar(1,"bar").asJson.as[Bar])
  //scalacOptions += "-Ymacro-annotations" is mandatory for JsonCodec to work
}

object SemiAutomaticDerivation3 extends App {



  case class User(id: Long, firstName: String, lastName: String)

  val user = User(1, "aamir", "fayaz")
  /*import io.circe._
  import io.circe.generic.semiauto._
  import io.circe.syntax._

  implicit val fooDecoder: Decoder[User] = deriveDecoder
  implicit val fooEncoder: Encoder[User] = deriveEncoder*/

  //without using generic derivations
  import io.circe.{Encoder, Decoder}

 implicit val decoderUser: Decoder[User] =
   Decoder.forProduct3("id", "first_name", "last_name")(User.apply)

  implicit val encoderUser: Encoder[User] =
    Encoder.forProduct3("id", "first_name", "last_name")(u =>
      (u.id, u.firstName, u.lastName)
    )

  import io.circe.syntax._
  println(user.asJson)

  println("----")
  println(user.asJson.as[User])
}*/
