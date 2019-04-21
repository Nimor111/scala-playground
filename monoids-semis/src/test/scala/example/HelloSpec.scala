package example

import org.scalatest._

import example._

class HelloSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    1 + 1 shouldEqual 2
  }
}
