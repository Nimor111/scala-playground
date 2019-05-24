package homework2

import scala.annotation.tailrec

sealed trait Validated[+E, +A] {
  def isValid: Boolean = this match {
    case Valid(_)   => true
    case Invalid(_) => false
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Valid(v)   => v
    case Invalid(_) => default
  }

  def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B] =
    this match {
      case Valid(_)   => this
      case Invalid(_) => default
    }

  def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)] = {
    this match {
      case Valid(v1) =>
        vb match {
          case Valid(v2)  => Valid((v1, v2))
          case Invalid(e) => Invalid(e)
        }
      case Invalid(e1) =>
        vb match {
          case Valid(_)    => Invalid(e1)
          case Invalid(e2) => Invalid(e1 ++ e2)
        }
    }
  }

  def map[B](f: A => B): Validated[E, B] = this match {
    case Valid(v)   => Valid(f(v))
    case Invalid(e) => Invalid(e)
  }

  def map2[EE >: E, B, R](vb: Validated[EE, B])(
      f: (A, B) => R): Validated[EE, R] = this match {
    case Valid(v1) =>
      vb match {
        case Valid(v2)  => Valid(f(v1, v2))
        case Invalid(e) => Invalid(e)
      }
    case Invalid(e1) =>
      vb match {
        case Valid(_)    => Invalid(e1)
        case Invalid(e2) => Invalid(e1 ++ e2)
      }
  }

  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] =
    this match {
      case Valid(v)   => f(v)
      case Invalid(e) => Invalid(e)
    }

  def fold[B](invalid: Chain[E] => B, valid: A => B): B = this match {
    case Invalid(errors) => invalid(errors)
    case Valid(a)        => valid(a)
  }

  def foreach(f: A => Unit): Unit = fold(_ => (), f)
}

case class Valid[+A](a: A) extends Validated[Nothing, A]
case class Invalid[+E](errors: Chain[E]) extends Validated[E, Nothing]

object Invalid {
  def apply[E](error: E): Invalid[E] = Invalid(Chain(error))
}

object Validated {
  def sequence[E, A](xs: List[Validated[E, A]]): Validated[E, List[A]] = {
    val (value, errors) = accumulate(xs, List(), None)

    errors match {
      case None =>
        value match {
          case list: List[A] => Valid(list)
        }
      case Some(e) => Invalid(e)
    }
  }

  @tailrec def accumulate[E](
      list: List[Any],
      value: List[Any],
      errors: Option[Chain[E]]): (List[Any], Option[Chain[E]]) = {
    list match {
      case Nil => (value, errors)
      case v :: vs =>
        v match {
          case Valid(vv) => accumulate(vs, value ++ List(vv), errors)
          case Invalid(e: Chain[E]) => {
            errors match {
              case None     => accumulate(vs, value, Some(e))
              case Some(e1) => accumulate(vs, value, Some(e1 ++ e))
            }
          }
        }
    }
  }

  implicit class ValidatedTuple2[EE, A, B](
      val tuple: (Validated[EE, A], Validated[EE, B]))
      extends AnyVal {
    def zip: Validated[EE, (A, B)] = tuple._1.zip(tuple._2)
    def zipMap[R](f: (A, B) => R): Validated[EE, R] = tuple._1.map2(tuple._2)(f)
  }

  implicit class ValidatedTuple3[EE, A, B, C](
      val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C]))
      extends AnyVal {
    def zip: Validated[EE, (A, B, C)] = {
      val (v1, v2, v3) = tuple
      val l = List(v1, v2, v3)

      val (value, errors) = accumulate(l, List(), None)

      errors match {
        case None =>
          value match {
            case List(a: A, b: B, c: C) => Valid((a, b, c))
          }
        case Some(e) => Invalid(e)
      }
    }

    def zipMap[R](f: (A, B, C) => R): Validated[EE, R] = {
      val (v1, v2, v3) = tuple
      val l = List(v1, v2, v3)

      val (value, errors) = accumulate(l, List(), None)

      errors match {
        case None =>
          value match {
            case List(a: A, b: B, c: C) => Valid(f(a, b, c))
          }
        case Some(e) => Invalid(e)
      }
    }
  }

  implicit class ValidatedTuple4[EE, A, B, C, D](
      val tuple: (Validated[EE, A],
                  Validated[EE, B],
                  Validated[EE, C],
                  Validated[EE, D]))
      extends AnyVal {
    def zip: Validated[EE, (A, B, C, D)] = {
      val (v1, v2, v3, v4) = tuple
      val l = List(v1, v2, v3, v4)

      val (value, errors) = accumulate(l, List(), None)

      errors match {
        case None =>
          value match {
            case List(a: A, b: B, c: C, d: D) => Valid((a, b, c, d))
          }
        case Some(e) => Invalid(e)
      }
    }

    def zipMap[R](f: (A, B, C, D) => R): Validated[EE, R] = {
      val (v1, v2, v3, v4) = tuple
      val l = List(v1, v2, v3, v4)

      val (value, errors) = accumulate(l, List(), None)

      errors match {
        case None =>
          value match {
            case List(a: A, b: B, c: C, d: D) => Valid(f(a, b, c, d))
          }
        case Some(e) => Invalid(e)
      }
    }
  }

  implicit class ValidatedTuple5[EE, A, B, C, D, E](
      val tuple: (Validated[EE, A],
                  Validated[EE, B],
                  Validated[EE, C],
                  Validated[EE, D],
                  Validated[EE, E]))
      extends AnyVal {
    def zip: Validated[EE, (A, B, C, D, E)] = {
      val (v1, v2, v3, v4, v5) = tuple
      val l = List(v1, v2, v3, v4, v5)

      val (value, errors) = accumulate(l, List(), None)

      errors match {
        case None =>
          value match {
            case List(a: A, b: B, c: C, d: D, e: E) => Valid((a, b, c, d, e))
          }
        case Some(e) => Invalid(e)
      }
    }
    def zipMap[R](f: (A, B, C, D, E) => R): Validated[EE, R] = {
      val (v1, v2, v3, v4, v5) = tuple
      val l = List(v1, v2, v3, v4, v5)

      val (value, errors) = accumulate(l, List(), None)

      errors match {
        case None =>
          value match {
            case List(a: A, b: B, c: C, d: D, e: E) => Valid(f(a, b, c, d, e))
          }
        case Some(e) => Invalid(e)
      }
    }
  }

  implicit class OptionToValidated[A](val op: Option[A]) {
    def toValidated[E](onEmpty: => E): Validated[E, A] = op match {
      case None    => Invalid(Singleton(onEmpty))
      case Some(v) => Valid(v)
    }
  }
}
