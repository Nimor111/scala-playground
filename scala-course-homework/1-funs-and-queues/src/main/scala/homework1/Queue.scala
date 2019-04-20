package homework1

class Queue(val pushed: List[Int], val popped: List[Int]) extends Iterable[Int] {
  override def iterator: Iterator[Int] = {
    (popped ::: pushed.reverse).iterator
  }

  def peek: Int = {
    popped match {
      case x :: _ => x
      case Nil => {
        pushed match {
          case Nil => throw new NoSuchElementException
          case xs => xs.last
        }
      }
    }
  }

  def push(n: Int): Queue = {
    new Queue(n :: pushed, popped)
  }

  def pop: Queue = {
    popped match {
      case Nil => pushed match {
        case Nil => throw new NoSuchElementException
        case xs => new Queue(Nil, xs.reverse.tail)
      }
      case _ :: xs => new Queue(pushed, xs)
    }
  }

  override def isEmpty: Boolean = {
    pushed.isEmpty && popped.isEmpty
  }

  override def size: Int = {
    pushed.size + popped.size
  }
}

object Queue {
  def empty: Queue = {
    new Queue(Nil, Nil)
  }

  def apply(xs: Seq[Int]): Queue = {
    new Queue(xs.toList.reverse, Nil)
  }
}
