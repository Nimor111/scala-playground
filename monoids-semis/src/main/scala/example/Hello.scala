package example

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = monoid
}

object IntMonoidInstance {
  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def combine(a: Int, b: Int) = a + b
    def empty = 0
  }
}

object BooleanMonoidInstance {
  implicit val booleanConjMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = a && b
      def empty = true
    }

  implicit val booleanDisjMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = a || b
      def empty = false
    }
}

object SetMonoidInstances {
  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]) = a union b
      def empty = Set.empty[A]
    }
}

object Hello {
  def main(args: Array[String]): Unit = {
    import BooleanMonoidInstance.booleanConjMonoid
    import IntMonoidInstance._

    println(combine(1, 2))
    println(combine(true, false))
  }

  private def combine[A](a1: A, a2: A)(implicit monoid: Monoid[A]): A = {
    return monoid.combine(a1, a2)
  }

  private def intUnionMonoid: Unit = {
    import SetMonoidInstances._

    val intUnionMonoid = Monoid[Set[Int]]

    println(intUnionMonoid.combine(Set(1, 2, 3), Set(4, 5, 6)))
  }
}
