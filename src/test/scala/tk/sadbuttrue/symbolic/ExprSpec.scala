package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * Created by true on 21/10/2016.
  */
class ExprSpec extends WordSpec with Matchers {
  "Expr" when {
    "eval" should {
      "be value of Number on number" in {
        eval(Number(10)) shouldEqual 10.0
      }
    }
  }
}
