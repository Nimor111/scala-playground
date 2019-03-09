package calculator

sealed trait TotalComputation

final case class Success(value: Double) extends TotalComputation
final case object Failure extends TotalComputation

object Ops {
  def +(first: TotalComputation, other: TotalComputation): TotalComputation = {
    first match {
      case Failure => return Failure
      case Success(v1) => {
        other match {
          case Failure => return Failure
          case Success(v2) => return Success(v1 + v2)
        }
      }
    }
  }

  def -(first: TotalComputation, other: TotalComputation): TotalComputation = {
    first match {
      case Failure => return Failure
      case Success(v1) => {
        other match {
          case Failure => return Failure
          case Success(v2) => return Success(v1 - v2)
        }
      }
    }
  }

  def /(first: TotalComputation, other: TotalComputation): TotalComputation = {
    first match {
      case Failure => return Failure
      case Success(v1) => {
        other match {
          case Failure => return Failure
          case Success(0) => return Failure
          case Success(v2) => return Success(v1 / v2)
        }
      }
    }
  }

  def sqrt(comp: TotalComputation): TotalComputation = {
    comp match {
      case Failure => return Failure
      case Success(v1) => {
        if(v1 < 0) {
          return Failure
        }

        return Success(Math.sqrt(v1))
      }
    }
  }
}

sealed trait Expr {
  def eval: TotalComputation = {
    this match {
      case Number(value) => Success(value)
      case Addition(left, right) => Ops.+(left.eval, right.eval)
      case Subtraction(left, right) => Ops.-(left.eval, right.eval)
      case Division(left, right) => Ops./(left.eval, right.eval)
      case SquareRoot(expr) => Ops.sqrt(expr.eval)
    }
  }
}

final case class Addition(left: Expr, right: Expr) extends Expr
final case class Subtraction(left: Expr, right: Expr) extends Expr
final case class Number(value: Double) extends Expr
final case class Division(left: Expr, right: Expr) extends Expr
final case class SquareRoot(expr: Expr) extends Expr
