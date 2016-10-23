package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * @author Eugene Aslanov
  */
class DivideSpec extends WordSpec with Matchers {
  "Divide" when {
    "0/0" should {
      "be NaN" in {
        val n = Number(0)
        eval(n / n).isNaN shouldBe true
      }
    }
    "/0" should {
      "be error" in {
        intercept[RuntimeException] {
          val n = Number(0)
          val v = Variable("x")
          v / n
        }
      }
    }
    "0/anything" should {
      "be 0" in {
        val n = Number(0)
        val v = Variable("x")
        n / v shouldEqual n
        eval(n / v) shouldEqual 0
      }
    }
    "v/1" should {
      "be v" in {
        val n = Number(1)
        val v = Variable("x")
        v / n shouldEqual v
      }
    }
    "x/y" should {
      "be frac(x, y)" in {
        Variable("x") / Variable("y") shouldEqual Frac(Variable("x"), Variable("y"))
      }
    }
  }
}
