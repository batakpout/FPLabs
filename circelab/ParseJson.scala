package circe_lab

import eu.timepit.refined.types.string.NonEmptyFiniteString
import io.circe._
import io.circe.parser._

import java.time.Instant
import java.util.UUID

object ParseJson extends App {

  val rawJson: String =
    """
  {
    "foo": "bar",
    "baz": 123,
    "list of stuff": [ 4, 5, 6 ]
  }
  """.stripMargin
  val parseResult: Either[ParsingFailure, Json] = parse(rawJson)
  println(parseResult)

  println("------")
  val badJson: String = "yolo"
  println(parse(badJson))

  /*
  parse(rawJson) match {
    case Left(failure) => println("Invalid JSON :(")
    case Right(json) => println("Yay, got some JSON!")
  }
  // Yay, got some JSON!
  Or use getOrElse(an extension method provided by Cats):

  val json: Json = parse(rawJson).getOrElse(Json.Null)
*/
}

object Test extends App {

  val json = """[
               |	{
               |		"uuid": "eebd6b85-68b1-ec11-a87f-005056b9e45a",
               |		"created_at": "2020-09-16T19:52:42+00:00",
               |		"updated_at": "2020-09-16T19:52:42+00:00",
               |		"name": {
               |			"eng": "Version1",
               |			"jpn": "Version1",
               |			"loc": "Version1"
               |		}
               |	},
               |	{
               |		"uuid": "f0bd6b85-68b1-ec11-a87f-005056b9e45a",
               |		"created_at": "2020-09-18T04:01:18+00:00",
               |		"updated_at": "2020-09-18T04:01:18+00:00",
               |		"name": {
               |			"eng": "Version_2",
               |			"jpn": "Version_2",
               |			"loc": "Version_2"
               |		}
               |	},
               |	{
               |		"uuid": "f2bd6b85-68b1-ec11-a87f-005056b9e45a",
               |		"created_at": "2020-09-18T04:23:39+00:00",
               |		"updated_at": "2020-09-18T04:23:39+00:00",
               |		"name": {
               |			"eng": "Version_012",
               |			"jpn": "Version_012",
               |			"loc": "Version_012"
               |		}
               |	},
               |	{
               |		"uuid": "f3bd6b85-68b1-ec11-a87f-005056b9e45a",
               |		"created_at": "2020-09-18T04:29:28+00:00",
               |		"updated_at": "2020-09-18T04:29:28+00:00",
               |		"name": {
               |			"eng": "Version_013",
               |			"jpn": "Version_013",
               |			"loc": "Version_013"
               |		}
               |	},
               |	{
               |		"uuid": "f5bd6b85-68b1-ec11-a87f-005056b9e45a",
               |		"created_at": "2020-09-18T05:09:35+00:00",
               |		"updated_at": "2020-09-18T05:09:35+00:00",
               |		"name": {
               |			"eng": "version_014",
               |			"jpn": "version_014",
               |			"loc": "version_014"
               |		}
               |	},
               |	{
               |		"uuid": "f6bd6b85-68b1-ec11-a87f-005056b9e45a",
               |		"created_at": "2020-09-18T05:13:23+00:00",
               |		"updated_at": "2020-09-18T05:13:23+00:00",
               |		"name": {
               |			"eng": "version_015",
               |			"jpn": "version_015",
               |			"loc": "version_015"
               |		}
               |	},
               |	{
               |		"uuid": "f8bd6b85-68b1-ec11-a87f-005056b9e45a",
               |		"created_at": "2020-09-18T12:27:08+00:00",
               |		"updated_at": "2020-09-18T12:27:08+00:00",
               |		"name": {
               |			"eng": "1.0",
               |			"jpn": "1.0",
               |			"loc": "1.0"
               |		}
               |	}
               |]""".stripMargin
  case class LocalisedScheduleName(eng: NonEmptyFiniteString[36])
  case class StudySchedule(
                            uuid: UUID,
                            createdAt: Instant,
                            updatedAt: Instant,
                            name: LocalisedScheduleName
                          )

  val parseResult: Either[ParsingFailure, Json] = parse(json)
  println(parseResult)
}

