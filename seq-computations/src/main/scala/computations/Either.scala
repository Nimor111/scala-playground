package computations

sealed trait Either[A, B] {
  def fold[C](acc: C, f: (B, C) => C): C = {
    this match {
      case Left(a) => acc
      case Right(b) => f(b, acc)
    }
  }
}

final case class Left[A, B](value: A) extends Either[A, B]
final case class Right[A, B](value: B) extends Either[A, B]
