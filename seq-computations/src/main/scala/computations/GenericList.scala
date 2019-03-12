package computations

sealed trait Result[T]

final case class Failure[T](msg: String) extends Result[T]
final case class Success[T](value: T) extends Result[T]

sealed trait GenericList[T] {
  def length: Int = {
    this match {
      case End() => 0
      case Pair(_, tail) => 1 + tail.length
    }
  }

  def contains(value: T): Boolean = {
    this match {
      case End() => false
      case Pair(head, tail) => {
        if(head == value) true else tail.contains(value)
      }
    }
  }

  def apply(n: Int): Result[T] = {
    this match {
      case End() => Failure("Element not found!")
      case Pair(head, tail) => {
        if (n == 0) Success(head) else tail.apply(n - 1)
      }
    }
  }
}

final case class Pair[T](head: T, tail: GenericList[T]) extends GenericList[T]
final case class End[T]() extends GenericList[T]
