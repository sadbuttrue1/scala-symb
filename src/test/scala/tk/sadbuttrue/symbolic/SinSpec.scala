package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * Created by true on 21/10/2016.
  */
class SinSpec extends WordSpec with Matchers {
  "Sin" when {
    "toString" should {
      "be correct with positive" in {
        Sin(Variable("x")).toString shouldEqual "sin(x)"
      }
      "be correct with negative" in {
        (-Sin(Variable("x"))).toString shouldEqual "-sin(x)"
      }
    }
  }
}
