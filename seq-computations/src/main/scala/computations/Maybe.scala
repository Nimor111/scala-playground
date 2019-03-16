package computations

sealed trait Maybe[+A] {
  def fold[B](acc: B, f: (A, B) => B): B = {
    this match {
      case Empty => acc
      case Just(a) => f(a, acc)
    }
  }
}

final case class Just[A](value: A) extends Maybe[A]
final case object Empty extends Maybe[Nothing]
