package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * Created by true on 21/10/2016.
  */
class PowerSpec extends WordSpec with Matchers {
  "Power" when {
    "2 ^ 10" should {
      "be 1024" in {
        eval(Number(2) ^ Number(10)) shouldEqual 1024d
      }
    }
    "2 ^ 0" should {
      "be 1" in {
        eval(Number(2) ^ 0) shouldEqual 1d
      }
    }
    "0 ^ 0" should {
      "be NaN" in {
        eval(Number(0) ^ 0).isNaN shouldBe true
      }
    }
  }
}
