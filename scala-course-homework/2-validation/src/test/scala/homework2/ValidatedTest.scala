package homework2

import org.scalatest.{ FlatSpec, FunSuite, Matchers }

class ValidatedTest extends FlatSpec with Matchers {
  "zip" should "combine valid instances" in {
    Valid(1).zip(Valid("a")) shouldEqual Valid((1, "a"))
  }

  it should "combine errors from invalid instances" in {
    Invalid(1).zip(Invalid(Chain(2, 3))) shouldEqual Invalid(Chain(1, 2, 3))
  }

  "map2" should "work with Valid instances" in {
    Valid(1).map2(Valid(2))(_ + _) shouldEqual Valid(3)
  }

  it should "stop on Invalid values" in {
    Valid(1).map2(Invalid(Chain(42)))(_ + _) shouldEqual Invalid(Chain(42))
  }

  it should "concatenate invalid values" in {
    Invalid(Chain("Invalid password"))
      .map2(Invalid(Chain("Invalid username")))((a: String, b: String) => a + b) shouldEqual Invalid(
        Chain("Invalid password", "Invalid username"))
  }

  "zip" should "work on 3-tuples" in {
    val res = (Valid(1), Valid("2"), Valid(3.0)).zip
    res shouldEqual Valid((1, "2", 3.0))
  }

  "toValidated" should "work on Option types" in {
    import homework2.Validated.OptionToValidated
    Some(1).toValidated("Field is empty") shouldEqual Valid(1)
    None.toValidated("Field is empty") shouldEqual Invalid("Field is empty")
  }
}
