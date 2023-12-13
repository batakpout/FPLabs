package leandro_cats.typeclasses

import java.io.FileOutputStream
import scala.util.Using

//Now, using implicits


trait ByteEncoder2[A] {
  def encode(a: A): Array[Byte]
}

trait Channel4 {
  def write[A](obj: A)(implicit en: ByteEncoder2[A])
}

object FileChannel4 extends Channel4 {
  override def write[A](obj: A)(implicit en: ByteEncoder2[A]): Unit = {
    Using(new FileOutputStream("/Users/awani/code_personal/FPlabs/files")) { os =>
      os.write(en.encode(obj))
      os.flush()
    }
  }
}

object ByteEncoder2 {
   implicit object StringInstance extends ByteEncoder2[String] {
     def encode(s: String) = s.getBytes()
   }
}

object OtherInstances {
  implicit object StringOtherInstance extends ByteEncoder2[String] {
    def encode(s: String) = s.map(b => (b + 3).toByte).toArray
  }
}
import OtherInstances._
object FileChannel4Test extends App {
  FileChannel4.write("little")
}
