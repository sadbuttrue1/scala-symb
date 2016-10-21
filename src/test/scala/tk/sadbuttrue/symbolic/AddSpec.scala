package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * Created by true on 21/10/2016.
  */
class AddSpec extends WordSpec with Matchers {
  "Add" when {
    "0 + 0" should {
      "be 0" in {
        eval(Number(0) + Number(0)) shouldEqual 0d
      }
    }
    "0 + expression" should {
      "be expression" in {
        Number(0) + Variable("x") shouldEqual Variable("x")
      }
    }
    "expression + 0" should {
      "be expression" in {
        Variable("x") + Number(0) shouldEqual Variable("x")
      }
    }
    "numbers a and b" should {
      "be number a+b" in {
        Number(9) + Number(21) shouldEqual Number(9 + 21)
      }
    }
    "a*x + b*x" should {
      "be (a+b)*x" in {
        Prod(Number(10), Variable("x")) + Prod(Number(11), Variable("x")) shouldEqual (Number(10) + Number(11)) * Variable("x")
      }
    }
    "any expressions x and y" should {
      "be sum of x and y" in {
        Variable("x") + Constant("c") shouldEqual Sum(Variable("x"), Constant("c"))
      }
    }
    "using ordinal numbers" should {
      "nor fail" in {
        val n = Number(10)
        eval(n + 10) shouldEqual 20d
        eval(10 + n) shouldEqual 20d
      }
    }
  }
}
