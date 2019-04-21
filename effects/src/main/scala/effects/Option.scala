package effects

sealed trait MyList[+A] {
  def head: Option[A] = {
    this match {
      case Empty => None
      case Cons(v, _) => Some(v)
    }
  }

  def tail: Option[MyList[A]] = {
    this match {
      case Empty => None
      case Cons(_, xs) => Some(xs)
    }
  }

  def map[B](f: A => B): MyList[B] = {
    this match {
      case Empty => Empty
      case Cons(v, xs) => Cons(f(v), xs.map(f))
    }
  }

  def filter(f: A => Boolean): MyList[A] = {
    this match {
      case Empty => Empty
      case Cons(v, xs) if f(v) => Cons(v, xs)
      case Cons(_, xs) => xs.filter(f)
    }
  }

  def foldRight[B](acc: B)(f: (A, B) => B): B = {
    this match {
      case Empty => acc
      case Cons(v, xs) => xs.foldRight(f(v, acc))(f)
    }
  }

  def prepend[AA >: A](value: AA): MyList[AA] = {
    this match {
      case Empty => Empty
      case Cons(v, xs) => Cons(value, Cons(v, xs))
    }
  }

  def nth(n: Int): Option[A] = {
    n match {
      case 0 => this match {
        case Empty => None
        case xs => xs.head
      }
      case k => this match {
        case Empty => None
        case Cons(_, xs) => xs.nth(k - 1)
      }
    }
  }

  override def toString: String = {
    this match {
      case Empty => ""
      case Cons(v, xs) => (v.toString + ", " + xs.toString).stripSuffix(", ")
    }
  }
}

case object Empty extends MyList[Nothing]
case class Cons[A](hd: A, tl: MyList[A]) extends MyList[A]
