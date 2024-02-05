package circe_lab

import io.circe.Decoder.{Result}

object TraverseModify1 extends App {

  import io.circe._
  import io.circe.parser._

  val json: String =
    """
    {
      "id": "c730433b-082c-4984-9d66-855c243266f0",
      "name": "Foo",
      "counts": [1, 2, 3],
      "values": {
        "bar": true,
        "baz": 100.001,
        "qux": ["a", "b"]
      }
    }
  """

  val doc: Json = parse(json).getOrElse(Json.Null)
  val cursor: HCursor = doc.hcursor
  val baz1: Result[Double] = cursor.downField("values").downField("baz").as[Double]
  val baz2: Result[Double] = cursor.downField("values").get[Double]("baz")
  val firstQux: Result[String] = cursor.downField("values").downField("qux").downArray.as[String]

  println(baz1)
  println(baz2)
  println(firstQux)

  //modify JSON

  val reversedNameCursor: ACursor = cursor.downField("name").withFocus(_.mapString(_.reverse))
  //We can then return to the root of the document and return its value with top
  val reversedName: Option[Json] = reversedNameCursor.top
  println(reversedName)
  println("------")

}