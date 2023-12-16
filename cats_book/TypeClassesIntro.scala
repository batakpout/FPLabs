// simple JSON AST
sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json

final case class JsString(get: String) extends Json

final case class JsNumber(get: Double) extends Json

final case object JsNull extends Json

// `serialize to Json` behaviour is encoded in this trait, a type class

trait JsonWriter[A] {
  def write(value: A): Json
}

case class Person(name: String, email: String)

//implicit values
object JsonWriterInstances {
  implicit val stringWriter = new JsonWriter[String] {
    override def write(value: String): Json = JsString(value)
  }

  implicit val doubleWriter = new JsonWriter[Double] {
    override def write(value: Double): Json = JsNumber(value)
  }

  implicit val personWriter = new JsonWriter[Person] {
    override def write(value: Person): Json = JsObject(Map(
      "name" -> JsString(value.name),
      "email" -> JsString(value.email)
    ))
  }
}

//Interface object
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

//interface syntax
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]) = //extension methods
      w.write(value)
  }
}

object TestJson1 extends App {


  import JsonWriterInstances._

  println(Json.toJson(Person("aamir", "abc@gmail.com")))
  import JsonSyntax._

  println(Person("aamir", "abc@gmail.com").toJson)

  println(implicitly[JsonWriter[Person]].write(Person("aamir", "abc@gmail.com"))) //Predef: def implicitly[T](implicit e: T): T = e


}

object TestJson2 extends App {

  //implicit scala.language.implicitConversions

  implicit def optionWriter[A](implicit w: JsonWriter[A]): JsonWriter[Option[A]] = new JsonWriter[Option[A]] {
    override def write(value: Option[A]): Json = value match {
      case Some(v) => w.write(v)
      case None => JsNull
    }
  }

  /** SAM- single abstract method
   * implicit def optionWriter[A](implicit w: JsonWriter[A]): JsonWriter[Option[A]] = (value: Option[A]) => value match {
   * case Some(v) => w.write(v)
   * case None => JsNull
   * }
   * Pattern matching anonymous function
   * implicit def optionWriter[A](implicit w: JsonWriter[A]): JsonWriter[Option[A]] = {
   * case Some(v) => w.write(v)
   * case None => JsNull
   * }
   */

  import JsonWriterInstances._
  println(Json.toJson(Option(10.2)))
}



