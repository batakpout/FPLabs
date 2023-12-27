
package cats_book

/**
 * Monoid for Boolean types.
 * AND, OR, XOR, XAND
 */

import cats.Monoid
import cats.Semigroup

object MonoidLaws {

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean =
    m.combine(m.combine(x, y), z) == m.combine(x, m.combine(y, z))

  def identityLaw[A](a: A)(implicit m: Monoid[A]): Boolean =
    (m.combine(a, m.empty) == a) && m.combine(m.empty, a) == a
}

object SemiGroupDefinition {
  trait Semigroup[A] {
    def combine(a: A, b: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }
}

object MonoidBooleanAndSetInstances {


  implicit val andOperation: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  implicit val orOperation: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  implicit val xOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
  }

  implicit val xNor: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)
  }

  implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]

    override def combine(x: Set[A], y: Set[A]): Set[A] = x.union(y)
  }

  implicit def setIntersectSemigroup[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x.union(y)
  }

  implicit def setSymmetricDiffMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]

    override def combine(x: Set[A], y: Set[A]): Set[A] = (x diff y) union (y diff x)
  }

}

object AddItemsMonoid {

  import cats.syntax.semigroup._

  def addItems[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  import cats.instances.option._

  val i1: List[Option[Int]] = List(Some(1), None, Some(2))
  addItems(i1) // here cats generate a Monoid[Option[Int]]

  val i2: List[Some[Int]] = List(Some(1), Some(2))
  //addItems(i2) //CTE here inferred type is List[Some[Int]] but cats only generate Monoid[Option[Int]]

  import cats.syntax.option._

  val i3: List[Option[Int]] = List(1.some, 2.some)
  addItems(i3)

  case class Order(totalCost: Double, quantity: Int)

  implicit val orderMonoid: Monoid[Order] = Monoid.instance[Order](
    Order(0.0, 0), { (o1, o2) =>
      Order(o1.totalCost + o2.totalCost, o1.quantity + o2.quantity)
    }
  )
}
