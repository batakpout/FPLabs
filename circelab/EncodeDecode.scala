package circe_lab

import io.circe
import io.circe.Decoder.Result
import io.circe.Json

object EncodeDecode1 extends App {

  import io.circe.syntax._

  //encode
  val intsJson1: Json = List(1,2,3,4).asJson
  println(intsJson1)

  //decode
  val intsJson2: Result[List[Int]] = intsJson1.as[List[Int]]
  println(intsJson2)

  import io.circe.parser.decode
  val decodedJson: Either[circe.Error, List[Int]] = decode[List[Int]]("[1,2,3]")
  println(decodedJson)

  val falseJsonDecoded: Either[circe.Error, List[Int]] = decode[List[Int]]("[1,2,3")
  println(falseJsonDecoded)
}