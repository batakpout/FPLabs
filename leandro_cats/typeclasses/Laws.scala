package leandro_cats.typeclasses

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.prop.Configuration
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.nio.ByteBuffer
import scala.util.Try

object Laws {

  //Property: Encoding & Decoding a value should return the original value unchanged.
  trait _ByteEncoder[A] {
    def encode(a: A): Array[Byte]
  }

  trait _ByteDecoder[A] {
    def decode(arr: Array[Byte]): Option[A]
  }

  trait ByteCodec[A] extends _ByteEncoder[A] with _ByteDecoder[A]

  object ByteCodec {
    implicit object IntByteCodec extends ByteCodec[Int] {
      override def encode(a: Int): Array[Byte] = {
        val bb = ByteBuffer.allocate(4)
        bb.putInt(a)
        bb.array()
      }

      override def decode(arr: Array[Byte]): Option[Int] = {
        if (arr.length != 4) None
        else {
          val bb = ByteBuffer.allocate(4)
          bb.put(arr)
          bb.flip() // since bb was in write mode, switch to read mode
          Some(bb.getInt)
        }
      }
    }

    implicit object StringByteCodec extends ByteCodec[String] {
      override def encode(s: String): Array[Byte] = s.getBytes

      override def decode(arr: Array[Byte]): Option[String] = Try(new String(arr)).toOption
    }
  }

  trait ByteCodecLaws[A] {
    def codec: ByteCodec[A]

    def isomorphism(a: A): Boolean =
      codec.decode(codec.encode(a)).contains(a)
  }

  trait ByteCodecTests[A] extends Laws {
    def laws: ByteCodecLaws[A]

    def byteCodec(implicit arb: Arbitrary[A]): RuleSet = new DefaultRuleSet(
      name = "bytecodec",
      parent = None,
      "isomorphism" -> forAll(laws.isomorphism _)
    )

  }

  object ByteCodecTests {
    def apply[A](implicit bc: ByteCodec[A]): ByteCodecTests[A] = new ByteCodecTests[A] {
      override def laws: ByteCodecLaws[A] = new ByteCodecLaws[A] {
        override def codec: ByteCodec[A] = bc
      }
    }
  }

  /*object IntByteCodecLaws extends ByteCodecLaws[Int] {
    override def codec: ByteCodec[Int] = IntByteCodec
  }

  object StringByteCodecLaws extends ByteCodecLaws[String] {
    override def codec: ByteCodec[String] = StringByteCodec
  }*/

  /*  object IntByteCodecTests extends ByteCodecTests[Int] {
      override def laws: ByteCodecLaws[Int] = IntByteCodecLaws
    }

    object StringByteCodecTests extends ByteCodecTests[String] {
      override def laws: ByteCodecLaws[String] = StringByteCodecLaws
    }*/


}

import Laws._

class ByteCodecSpec extends AnyFunSuiteLike with Configuration with FunSuiteDiscipline {
  checkAll("ByteCodec[Int]", ByteCodecTests[Int].byteCodec)
  checkAll("ByteCodec[String]", ByteCodecTests[String].byteCodec)
}
