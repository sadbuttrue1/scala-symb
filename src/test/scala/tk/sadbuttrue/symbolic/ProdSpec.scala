package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * Created by true on 21/10/2016.
  */
class ProdSpec extends WordSpec with Matchers {
  "Prod" when {
    "toString" should {
      "be (sum)*(sum) for sum multiplication" in {
        ((Constant("B") + Number(10)) * (Variable("x") + Constant("C"))).toString shouldEqual "(B+10.0)*(x+C)"
      }
      "be (sub)*(sub) for sum multiplication" in {
        ((Constant("B") - Number(10)) * (Variable("x") - Constant("C"))).toString shouldEqual "(B-10.0)*(x-C)"
      }
      "be expr*expr for any other expression" in {
        (Number(10) * Variable("x")).toString shouldEqual "10.0*x"
      }
    }
  }
}
