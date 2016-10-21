package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * Created by true on 21/10/2016.
  */
class FracSpec extends WordSpec with Matchers {
  "Frac" when {
    "toString" should {
      "convert correct" in {
        (Variable("x") / Constant("C")).toString shouldEqual "(x)/(C)"
      }
    }
  }
}
