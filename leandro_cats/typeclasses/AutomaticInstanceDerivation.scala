package leandro_cats.typeclasses

object AutomaticInstanceDerivation extends App {

  import Laws._

  implicit def optionByteEncoder[A](implicit be: _ByteEncoder[A]): _ByteEncoder[Option[A]]
  = new _ByteEncoder[Option[A]] {
    override def encode(a: Option[A]): Array[Byte] = a match {
      case Some(v) => be.encode(v)
      case None => Array.empty[Byte]
    }
  }
  import cats.syntax.option._
  _ByteEncoder[Option[String]].encode("Hi".some)
}
