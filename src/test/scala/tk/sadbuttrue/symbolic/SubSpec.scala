package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * Created by true on 21/10/2016.
  */
class SubSpec extends WordSpec with Matchers {
  "Sub" when {
    "0 - 0" should {
      "be 0" in {
        eval(Number(0) - Number(0)) shouldEqual 0d
      }
    }
    "0 - expression" should {
      "be -expression" in {
        Number(0) - Variable("x") shouldEqual Variable("x", true)
      }
    }
    "expression - 0" should {
      "be expression" in {
        Variable("x") - Number(0) shouldEqual Variable("x")
      }
    }
    "numbers a and b" should {
      "be number a-b" in {
        Number(9) - Number(21) shouldEqual Number(9 - 21)
      }
    }
    "a*x - b*x" should {
      "be (a-b)*x" in {
        Prod(Number(10), Variable("x", true)) - Prod(Number(11), Variable("x", true)) shouldEqual (Number(10) - Number(11)) * Variable("x", true)
      }
    }
    "any expressions x and y" should {
      "be sub of x and y" in {
        Variable("x") - Constant("c") shouldEqual Sub(Variable("x"), Constant("c"))
      }
    }
    "expression and sub" should {
      "be expression + minus sub" in {
        Variable("x") - Sub(Number(10), Variable("y")) shouldEqual Variable("x") + (-Sub(Number(10), Variable("y")))
      }
    }
    "using ordinal numbers" should {
      "nor fail" in {
        val n = Number(10)
        eval(n - 10) shouldEqual 0d
        eval(10 - n) shouldEqual 0d
      }
    }
  }
}
