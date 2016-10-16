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
