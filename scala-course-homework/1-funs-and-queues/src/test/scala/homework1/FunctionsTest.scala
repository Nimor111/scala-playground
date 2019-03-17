package homework1

import org.scalatest.{FlatSpec, Matchers}
import Functions._

class FunctionsTest extends FlatSpec with Matchers {
  "fromDigits" should "form a decimal number" in {
    fromDigits(List(1, 2, 3)) shouldBe 123
  }

  it should "form a hex number" in {
    fromDigits(List(1, 12, 4), 16) shouldBe 452
  }

  it should "form a binary number" in {
    fromDigits(List(1, 0, 1), 2) shouldBe 5
  }

  it should "handle empty list" in {
    fromDigits(List()) shouldBe 0
  }

  "parseInteger" should "parse a decimal number" in {
    parseInteger("123") shouldBe 123
  }

  it should "parse a hex number" in {
    parseInteger("1C4", 16) shouldBe 452
  }

  it should "handle empty string" in {
    parseInteger("") shouldBe 0
  }

  it should "parse a binary number" in {
    parseInteger("0111001", 2) shouldBe 57
  }

  it should "handle negative numbers" in {
    parseInteger("-1C4", 16) shouldBe -452
    parseInteger("-123") shouldBe -123
    parseInteger("-0111001", 2) shouldBe -57
  }

  "zipMap" should "transform two lists" in {
    zipMap(List(1, 2, 3), List(4, 5, 6), _ * _) shouldBe List(4, 10, 18)
  }

  it should "handle lists with different size" in {
    zipMap(List(3, 6), List(20, 30, 40), (x, y) => y - x) shouldBe List(17, 24)
  }

  "countCoinChangeVariants" should "count the ways to give a change" in {
    countCoinChangeVariants(List(1, 2, 5), 6) shouldBe 5
  }
}
