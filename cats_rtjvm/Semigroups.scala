package cats_rtjvm

import cats.Semigroup
import cats.instances.int._
import cats.instances.option._

object Semigroups extends App {

  val intSG = Semigroup[Int].combine(1, 2)
  val stringSG = Semigroup[String].combine("aamir", " fayaz")
  val optionsSG = Semigroup[Option[Int]].combine(Some(1), None)

  println(intSG)
  println(stringSG)
  println(optionsSG)

  def reduceInts(ints: List[Int]): Int = ints.reduce(Semigroup[Int].combine)

  def reduceStrings(strings: List[String]): String = strings.reduce(Semigroup[String].combine)

  def genericReduce[A](list: List[A])(implicit sG: Semigroup[A]) = list.reduce(sG.combine)

  println(reduceInts(List(1, 2, 3)))
  println(genericReduce(List(1, 2, 3)))
  println(genericReduce(List(Some(1), None, Some(2))))

  case class Expense(id: Long, amount: Double)

  implicit val expenseSG = Semigroup.instance[Expense] { (exp1, exp2) =>
    Expense(Math.max(exp1.id, exp2.id), exp1.amount + exp2.amount)
  }
  println(genericReduce(List(Expense(1, 11), Expense(2, 12), Expense(3, 12.3))))

  //extension methods
  import cats.syntax.semigroup._
  val s = 1 |+| 2
  val e = Expense(1,11.2) |+| Expense(2, 22.2)

  /*trait SemiGroupSyntax {
    implicit final def convertTo[A: Semigroup](a: A) = {
      new SemigroupOps[A](a)
    }
    final class SemigroupOps[A : Semigroup](lhs: A) {
      def |+|(rhs: A):A = Semigroup[A].combine(lhs, rhs)
    }
  }*/

  def genericReduce2[A : Semigroup](list: List[A]) = list.reduce(_ |+| _)
  // A: Semigroup; type-context

}
