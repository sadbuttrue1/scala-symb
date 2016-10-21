package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * Created by true on 21/10/2016.
  */
class ConstantSpec extends WordSpec with Matchers {
  "Constant" when {
    "toString" should {
      "be correct with positive" in {
        Constant("c").toString shouldEqual "c"
      }
      "be correct with negative" in {
        (-Constant("c")).toString shouldEqual "-c"
      }
    }
  }
}
