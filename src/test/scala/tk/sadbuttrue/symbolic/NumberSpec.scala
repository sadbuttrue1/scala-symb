package tk.sadbuttrue.symbolic

import org.scalatest._
import Expr.eval

/**
  * Created by true on 06/03/16.
  */
class NumberSpec extends WordSpec with Matchers {
  "Number" when {
    "add" should {
      "9 + 1 = 10" in {
        val n1 = Number(9)
        val n2 = Number(1)
        eval(n1 + n2) shouldEqual 10d
      }
    }
    "sub" should {
      "9 - 1 = 8" in {
        val n1 = Number(9)
        val n2 = Number(1)
        eval(n1 - n2) shouldEqual 8d
      }
    }
    "multiply" should {
      "be 0 if multiply on 0" in {
        val n1 = Number(0)
        val n2 = Number(9)
        eval(n1 * n2) shouldEqual 0d
        eval(n2 * n1) shouldEqual 0d
      }
      "be expression if multiply on 1" in {
        val n1 = Number(1)
        val n2 = Number(9)
        val v1 = Variable("x")
        eval(n1 * n2) shouldEqual 9d
        eval(n2 * n1) shouldEqual 9d
        n1 * v1 shouldEqual v1
        v1 * n1 shouldEqual v1
      }
      "2 * 9 = 18" in {
        val n1 = Number(2)
        val n2 = Number(9)
        eval(n1 * n2) shouldEqual 18d
        n1 * n2 shouldEqual Number(18)
      }
    }
    "derive" should {
      "return 0 with any variable" in {
        eval(Number(10) derive Variable("x")) shouldEqual 0d
      }
    }
    "negotiate" should {
      "work with positive" in {
        val n = Number(10)
        eval(-n) shouldEqual -10
        eval(-(-n)) shouldEqual 10
      }
      "work with negative" in {
        val n = Number(-10)
        eval(-n) shouldEqual 10
        eval(-(-n)) shouldEqual -10
      }
    }
  }
}
