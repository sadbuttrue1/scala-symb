package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * Created by true on 21/10/2016.
  */
class PowerSpec extends WordSpec with Matchers {
  "Power" when {
    "2 ^ 10" should {
      "be 1024" in {
        eval(Number(2) ^ Number(10)) shouldEqual 1024d
      }
    }
    "2 ^ 0" should {
      "be 1" in {
        eval(Number(2) ^ 0) shouldEqual 1d
      }
    }
    "2 ^ 1" should {
      "be 2" in {
        Number(2) ^ Number(1) shouldEqual Number(2)
      }
    }
    "0 ^ 0" should {
      "be NaN" in {
        eval(Number(0) ^ 0).isNaN shouldBe true
      }
    }
    "toString" should {
      "be correct with positive" in {
        (Variable("x") ^ Number(10)).toString shouldEqual Variable("x").toString + "^" + 10d.toString
      }
      "be correct with negative" in {
        (-(Variable("x") ^ Number(10))).toString shouldEqual "-" + Variable("x").toString + "^" + 10d.toString
      }
    }
  }
}
