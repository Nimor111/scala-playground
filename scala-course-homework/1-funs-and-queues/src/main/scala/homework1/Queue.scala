package homework1

class Queue(val pushed: List[Int], val popped: List[Int]) {
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
      case Nil => throw new NoSuchElementException
      case _ :: xs => new Queue(pushed, xs)
    }
  }

  def isEmpty: Boolean = {
    pushed.isEmpty && popped.isEmpty
  }

  def size: Int = {
    pushed.size + popped.size
  }
}

object Queue {
  def empty: Queue = {
    new Queue(Nil, Nil)
  }

  def apply(xs: Seq[Int]): Queue = {
    new Queue(Nil, xs.toList)
  }
}
