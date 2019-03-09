package calculator

import org.scalatest._

class ExprSpec extends FlatSpec with Matchers {
  "The Expr object" should "handle successful computations" in {
    Addition(SquareRoot(Number(4.0)), Number(2.0)).eval == Success(4.0)
  }

  it should "handle failing computations" in {
    Division(Number(4), Number(0)).eval shouldEqual Failure
    Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval shouldEqual Failure
  }
}
