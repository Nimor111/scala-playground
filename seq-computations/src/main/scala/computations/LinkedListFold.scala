package computations

sealed trait LinkedListFold {
  def fold(acc: Int, f: (Int, Int) => Int): Int = {
    this match {
      case EndF => acc
      case PairF(head, tail) => f(head, tail.fold(acc, f))
    }
  }

  def length: Int = {
    fold(0, (elem: Int, acc: Int) => acc + 1)
  }

  def sum: Int = {
    fold(0, (elem: Int, acc: Int) => acc + elem)
  }

  def product: Int = {
    fold(1, (elem: Int, acc: Int) => acc * elem)
  }
}

final case class PairF(head: Int, tail: LinkedListFold) extends LinkedListFold
final case object EndF extends LinkedListFold
