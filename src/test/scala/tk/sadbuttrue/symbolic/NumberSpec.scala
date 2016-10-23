package tk.sadbuttrue.symbolic

import org.scalatest._
import Expr.eval
import Expr.doubleToNumber

/**
  * Created by true on 06/03/16.
  */
class NumberSpec extends WordSpec with Matchers {
  "Number" when {
    "derive" should {
      "return 0 with any variable" in {
        eval(Number(10) derive Variable("x")) shouldEqual 0d
      }
    }
    "toString" should {
      "convert correct" in {
        Number(10).toString shouldEqual "10.0"
      }
    }
  }
}
