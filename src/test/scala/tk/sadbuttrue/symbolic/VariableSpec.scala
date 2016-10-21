package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * Created by true on 21/10/2016.
  */
class VariableSpec extends WordSpec with Matchers {
  "Variable" when {
    "toString" should {
      "be correct with positive" in {
        Variable("x").toString shouldEqual "x"
      }
      "be correct with negative" in {
        (-Variable("x")).toString shouldEqual "-x"
      }
    }
  }
}
