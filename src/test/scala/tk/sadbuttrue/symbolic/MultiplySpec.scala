package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * @author Eugene Aslanov
  */
class MultiplySpec extends WordSpec with Matchers {
  "Multiply" when {
    "zero and something" should {
      "be zero" in {
        Number(0) * Variable("x") shouldEqual Number(0)
        Variable("x") * Number(0) shouldEqual Number(0)
      }
    }
    "one and something" should {
      "be something" in {
        Number(1) * Variable("x") shouldEqual Variable("x")
        Variable("x") * Number(1) shouldEqual Variable("x")
      }
    }
    "two numbers" should {
      "be number x*y" in {
        Number(10) * Number(2) shouldEqual Number(20)
      }
    }
    "variable and number" should {
      "be number and variable" in {
        Variable("x") * Number(10) shouldEqual Number(10) * Variable("x")
      }
    }
    "variable and constant" should {
      "be constant and variable" in {
        Variable("x") * Constant("c") shouldEqual Constant("c") * Variable("x")
      }
    }
    "anything and anything" should {
      "be prod" in {
        Variable("x") * Variable("y") shouldEqual Prod(Variable("x"), Variable("y"))
      }
    }
  }
}
