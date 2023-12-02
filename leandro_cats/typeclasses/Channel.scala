package leandro_cats.typeclasses

import java.nio.ByteBuffer

import java.io.FileOutputStream
import scala.util.Using
/**
  type classes are often considered a more flexible and composable alternative to
  traditional inheritance for designing polymorphic behavior.
 */
trait Channel {
  def write(obj: Any): Unit
}

object FileChannel extends Channel {
  override def write(obj: Any): Unit = {

    val bytes: Array[Byte] = obj match {
      case n: Int =>
        val bb: ByteBuffer = ByteBuffer.allocate(4)
        bb.putInt(n)
        bb.array()
      case s: String =>
        s.getBytes()
      // s.map(_.toByte).toArray
      case _ => throw new Exception("undefined")
    }

    Using(new FileOutputStream("/Users/awani/code_personal/FPlabs/files")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

object TestFileChannel extends App {
  FileChannel.write("hello")
}

/**
 * This approach above has pros and cons
 * Pros: Very Simple Interface
 * Cons: unhandled types --> throw exception and two responsibilities (1. Getting Bytes, 2. Writing Bytes)
   ideally we should have only one responsibility, we should care about one thing only
   How can we improve? Inheritance to the rescue
 */

trait ByteEncodable {
  def encode(): Array[Byte]
}

trait Channel2 {
  def write(obj: ByteEncodable): Unit
}

object FileChannel2 extends Channel2 {
  override def write(obj: ByteEncodable): Unit = {
    Using(new FileOutputStream("/Users/awani/code_personal/FPlabs/files")) { os =>
      os.write(obj.encode())
      os.flush()
    }
  }
}

object TestFileChannel2 extends App {
  case class Name(firstName: String, lastName: String) extends ByteEncodable {
    override def encode(): Array[Byte] = firstName.getBytes ++ lastName.getBytes
  }

  FileChannel2.write(Name("aamir", "fayaz"))
}

/**
 * Again inheritance approach above has pros and cons
 * Pros:
    - We now have a unique responsibility in the Channel
    - easy to test
    - unhandled type (that is not byte-encodable) -> compile error
 * Cons:
   - how do we extend classes we don't have control over e.g how do we extend Int
   - only one implementation we can provide as we can extend ByteEncodable once only
   - overload interfaces i.e put extra method in the classes (e.g Name here) we extend ByteEncodable,
     what if it has lot of other methods it extends from other classes, so things r gettings complicated
 */

 //type classes to the rescue

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

trait Channel3 {
  def write[A](obj: A, en: ByteEncoder[A])
}

object IntByteEncoder extends ByteEncoder[Int] {
  override def encode(a: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(a)
    bb.array()
  }
}

object StringByteEncoder extends ByteEncoder[String] {
  override def encode(a: String): Array[Byte] =
    a.getBytes()
}

object FileChannel3 extends Channel3 {
  override def write[A](obj: A, en: ByteEncoder[A]): Unit = {
    Using(new FileOutputStream("/Users/awani/code_personal/FPlabs/files")) { os =>
      os.write(en.encode(obj))
      os.flush()
    }
  }
}

object FileChannel3Test extends App {
   FileChannel3.write(3, IntByteEncoder)
   FileChannel3.write("Hello", StringByteEncoder)
}

/**
  Advantages of using type classes:
  - can be instantiated by any type (custom, Int etc)
  - cleaner interface [no extending classes, we inject instances from the outside], so type remains the same
  - several implementations possible for the same type 
 */
