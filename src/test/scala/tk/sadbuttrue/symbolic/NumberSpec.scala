package tk.sadbuttrue.symbolic

import org.scalatest._
import Expr._

/**
  * Created by true on 06/03/16.
  */
class NumberSpec extends WordSpec with Matchers {
  "Number" when {
    "toString" should {
      "convert correct" in {
        Number(10).toString shouldEqual "10.0"
      }
    }
  }
}
