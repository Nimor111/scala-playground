package traits

sealed trait IntList {
  def length: Int = {
    this match {
      case End           => 0
      case Cons(_, tail) => 1 + tail.length
    }
  }

  def product: Int = {
    this match {
      case End              => 1
      case Cons(head, tail) => head * tail.product
    }
  }

  def double: IntList = {
    this match {
      case End              => End
      case Cons(head, tail) => Cons(head * 2, tail.double)
    }
  }
}

final case object End extends IntList
final case class Cons(head: Int, tail: IntList) extends IntList
