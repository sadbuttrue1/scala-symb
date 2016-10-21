package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by true on 21/10/2016.
  */
class CosSpec extends WordSpec with Matchers {
  "Cos" when {
    "toString" should {
      "be correct with positive" in {
        Cos(Variable("x")).toString shouldEqual "cos(x)"
      }
      "be correct with negative" in {
        (-Cos(Variable("x"))).toString shouldEqual "-cos(x)"
      }
    }
  }
}
