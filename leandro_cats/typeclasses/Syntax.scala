package leandro_cats.typeclasses

object Syntax extends App {

  import Laws._

  object _ByteEncoderSyntax {
    implicit class _ByteEncoderOps[A](val a: A) extends AnyVal {
      def encode(implicit be: _ByteEncoder[A]): Array[Byte] =
        be.encode(a)
    }
  }

  object _ByteDecoderSyntax {
    implicit class _ByteDecoderOps[A](a: Array[Byte]) {
      def decode(implicit be: _ByteDecoder[A]): Option[A] =
        be.decode(a)
    }
  }

  import _ByteEncoderSyntax._
  import _ByteDecoderSyntax._

  "Hello".encode.decode

  /**
    Syntax may affect performance a little bit becoz everytime it has to create an instance using new
    a work around is to make Syntax class as Value class so compiler will optimize implicit classes
    and we don't have to pay the price of extra allocation, so it helps us write more performant code.
    So if we do this, extra allocation won't happen.
   */
}
