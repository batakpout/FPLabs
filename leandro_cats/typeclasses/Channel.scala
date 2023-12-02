package leandro_cats.typeclasses

import java.nio.ByteBuffer

import java.io.FileOutputStream
import scala.util.Using

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
 * ideally we should have only one responsibility, we should care about one thing only
 * How can we improve? Inheritance to the rescue
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
