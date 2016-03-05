package tk.sadbuttrue.symbolic

import org.scalatest._
import Expr.eval

/**
  * Created by true on 06/03/16.
  */
class NumberSpec extends WordSpec with Matchers {
  "Number" when {
    "add" should {
      "two numbers value" in {
        val n1 = Number(9)
        val n2 = Number(1)
        eval(n1 + n2) shouldEqual 10d
      }
      "two numbers text" in {
        val n1 = Number(9)
        val n2 = Number(1)
        ((n1 + n2) toString) shouldEqual "10.0"
      }
    }
    "derive" should {
      "return 0" in {
        eval(Number(10) derive Variable("x")) shouldEqual 0d
      }
    }
    "negotiate" should {
      "works with positive" in {
        val n = Number(10)
        eval(-n) shouldEqual -10
        eval(-(-n)) shouldEqual 10
      }
      "works with negative" in {
        val n = Number(-10)
        eval(-n) shouldEqual 10
        eval(-(-n)) shouldEqual -10
      }
    }
  }
}
