
package cats_book

object MonadExample1 extends App {

  def parseInt(a: String): Option[Int] =
    a.toIntOption

  def divide(a: Int, b: Int): Option[Int] =
    if(b == 0) None else Some(a / b)

  def stringDivideBy(s1: String, s2: String): Option[Int] =
    parseInt(s1).flatMap { i1 =>
      parseInt(s2).flatMap { i2 =>
        divide(i1, i2)
      }
    }

   println(stringDivideBy("10", "2"))
   println(stringDivideBy("10", "aa"))

  def stringDivideByFC(s1: String, s2: String): Option[Int] =
    for {
      i1 <- parseInt(s1)
      i2 <- parseInt(s2)
      r <- divide(i1, i2)
    } yield r
}
