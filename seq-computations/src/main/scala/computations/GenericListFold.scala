package computations

sealed trait GenericListFold[T] {
  def fold[U](acc: U, f: (T, U) => U): U = {
    this match {
      case EndF() => acc
      case PairF(head, tail) => tail.fold(f(head, acc), f)
      // case PairF(head, tail) => f(head, tail.fold(acc, f))
    }
  }

  // def fold(acc: Int, f: (Int, Int) => Int): Int = {
  //   this match {
  //     case EndF() => acc
  //     case PairF(head, tail) => tail.fold(f(head, acc), f)
  //     // case PairF(head, tail) => f(head, tail.fold(acc, f))
  //   }
  // }

  def length: Int = {
    fold(0, (elem: T, acc: Int) => acc + 1)
  }

  // def sum: Int = {
  //   fold(0, (elem: Int, acc: Int) => acc + elem)
  // }
}

final case class PairF[T](head: T, tail: GenericListFold[T]) extends GenericListFold[T]
final case class EndF[T]() extends GenericListFold[T]
