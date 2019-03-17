package homework1

class Queue(val queue: List[Int]) {
  def peek: Int = {
    queue.isEmpty match {
      case true => throw new NoSuchElementException
      case false => queue(0)
    }
  }

  def push(n: Int): Queue = {
    new Queue(queue ::: List(n))
  }

  def pop: Queue = {
    queue match {
      case List() => throw new NoSuchElementException
      case _ :: xs => new Queue(xs)
    }
  }

  def isEmpty: Boolean = {
    queue.isEmpty
  }

  def size: Int = {
    queue.size
  }
}

object Queue {
  def empty: Queue = {
    new Queue(List())
  }

  def apply(xs: Seq[Int]): Queue = {
    new Queue(xs.toList)
  }
}
