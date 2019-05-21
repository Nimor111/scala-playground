package homework2

sealed trait Chain[+A] {
  def head: A
  def tail: Option[Chain[A]]

  def isEmpty: Boolean = false

  def +:[B >: A](front: B): Chain[B] = this match {
    case Singleton(h) => Append(Singleton(front), Singleton(h))
    case Append(l, r) => Append(l.+:(front), r)
  }

  def :+[B >: A](back: B): Chain[B] = this match {
    case Singleton(h) => Append(Singleton(h), Singleton(back))
    case Append(l, r) => Append(l, r :+ back)
  }

  def ++[B >: A](right: Chain[B]): Chain[B] = this match {
    case Singleton(h) => Append(Singleton(h), right)
    case Append(l, r) => Append(Append(l, r), right)
  }

  def foldLeft[B](initial: B)(f: (B, A) => B): B = this.listify match {
    case Singleton(h) => f(initial, h)
    case Append(Singleton(h), t) => t.foldLeft(f(initial, h))(f)
    case _ => sys.error("Unexpected listify format")
  }

  def reduceLeft[B >: A](f: (B, A) => B): B = this.listify match {
    case Singleton(first) => first
    case Append(Singleton(first), rest) => rest.foldLeft(first: B)(f)
    case _ => sys.error("Unexpected listify format")
  }

  def map[B](f: A => B): Chain[B] = this.listify match {
    case Singleton(first) => Singleton(f(first))
    case Append(Singleton(h), t) => f(h) +: t.map(f)
    case _ => sys.error("Unexpected listify format")
  }

  def flatMap[B](f: A => Chain[B]): Chain[B] = this.listify match {
    case Singleton(first) => f(first)
    case Append(Singleton(h), t) => f(h) ++ t.flatMap(f)
    case _ => sys.error("Unexpected listify format")
  }

  def foreach(f: A => Unit): Unit = foldLeft(())((_, next) => f(next))

  override def equals(that: Any): Boolean = that match {
    case c: Chain[_] => c.toList == this.toList
    case _ => false
  }

  override def hashCode: Int = foldLeft(0) {
    _ * 31 + _.hashCode
  }

  override def toString: String = toList.mkString("Chain(", ",", ")")

  def toList: List[A] =
    foldLeft(List.empty[A])((acc, next) => next :: acc).reverse
  def toSet[B >: A]: Set[B] = foldLeft(Set.empty[B])((acc, next) => acc + next)

  def min[B >: A](implicit order: Ordering[B]): B = {
    val first: B = this.head
    foldLeft(first)(order.min)
  }

  def max[B >: A](implicit order: Ordering[B]): B = {
    val first: B = this.head
    foldLeft(first)(order.max)
  }

  def listify: Chain[A] = this match {
    case Singleton(h) => Singleton(h)
    case Append(Singleton(h), r) => Singleton(h) ++ r.listify
    case Append(l, r) =>
      l.tail match {
        case None => l ++ r.listify
        case Some(t) => Singleton(l.head) ++ Append(t, r).listify
      }
  }
}

case class Singleton[+A](head: A) extends Chain[A] {
  def tail: Option[Chain[A]] = None
}
case class Append[+A](left: Chain[A], right: Chain[A]) extends Chain[A] {
  def head: A = left.head
  def tail: Option[Chain[A]] = left match {
    case Singleton(_) => Some(right)
    case _ => listify.tail
  }
}

object Chain {
  def apply[A](head: A, rest: A*): Chain[A] = {
    val acc: Chain[A] = Singleton(head)
    rest.foldLeft(acc)((acc: Chain[A], e: A) => acc :+ e)
  }

  // Allows Chain to be used in pattern matching
  def unapplySeq[A](chain: Chain[A]): Option[Seq[A]] = Some(chain.toList)
}
