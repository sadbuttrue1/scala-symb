package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * @author Eugene Aslanov
  */
class NegativeSpec extends WordSpec with Matchers {
  "Negative" when {
    "number" should {
      "be inverted" in {
        -Number(10) shouldEqual Number(-10)
      }
    }
    "variable" should {
      "be inverted" in {
        -Variable("x") shouldEqual Variable("x", true)
      }
    }
    "constant" should {
      "be inverted" in {
        -Constant("c") shouldEqual Constant("c", true)
      }
    }
    "product" should {
      "be inverted" in {
        -Prod(Variable("x"), Constant("c")) shouldEqual Variable("x", true) * Constant("c")
      }
    }
    "frac" should {
      "be inverted" in {
        -Frac(Variable("x"), Constant("c")) shouldEqual Variable("x", true) / Constant("c")
      }
    }
    "sum" should {
      "be inverted" in {
        -Sum(Variable("x"), Constant("c")) shouldEqual Variable("x", true) + Constant("c", true)
      }
    }
    "sub" should {
      "be inverted" in {
        -Sub(Variable("x"), Constant("c")) shouldEqual Constant("c") - Variable("x")
      }
    }
    "power" should {
      "be inverted" in {
        -(Variable("x") ^ Number(10)) shouldEqual Power(Variable("x"), 10, true)
      }
    }
    "sin" should {
      "be inverted" in {
        -Sin(Variable("x")) shouldEqual Sin(Variable("x"), true)
      }
    }
    "cos" should {
      "be inverted" in {
        -Cos(Variable("x")) shouldEqual Cos(Variable("x"), true)
      }
    }
  }
}
