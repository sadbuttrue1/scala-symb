package tk.sadbuttrue.symbolic

import org.scalatest.{Matchers, WordSpec}
import Expr._

/**
  * @author Eugene Aslanov
  */
class DeriveSpec extends WordSpec with Matchers {
  "Derive" when {
    "number" should {
      "be zero" in {
        Number(10).derive(Variable("x")) shouldEqual Number(0)
      }
    }
    "constant" should {
      "be zero" in {
        Constant("c").derive(Variable("x")) shouldEqual Number(0)
      }
    }
    "variable" should {
      "be one if same" in {
        Variable("x").derive(Variable("x")) shouldEqual Number(1)
      }
      "be zero if different" in {
        Variable("y").derive(Variable("x")) shouldEqual Number(0)
      }
    }
    "sum" should {
      "be sum of derivatives" in {
        ((Variable("x") ^ Number(3)) + (Variable("x") ^ Number(5))) derive Variable("x") shouldEqual (Variable("x") ^ Number(3)).derive(Variable("x")) + (Variable("x") ^ Number(5)).derive(Variable("x"))
      }
    }
    "sub" should {
      "be sub of derivatives" in {
        ((Variable("x") ^ Number(3)) - (Variable("x") ^ Number(5))) derive Variable("x") shouldEqual (Variable("x") ^ Number(3)).derive(Variable("x")) - (Variable("x") ^ Number(5)).derive(Variable("x"))
      }
    }
    "multiply" should {
      "be correct" in {
        ((Variable("x") ^ Number(3)) * (Variable("x") ^ Number(5))) derive Variable("x") shouldEqual ((Variable("x") ^ Number(3)) * (Variable("x") ^ Number(5)).derive(Variable("x"))) + ((Variable("x") ^ Number(5)) * (Variable("x") ^ Number(3)).derive(Variable("x")))
      }
    }
    "power" should {
      "be zero if n==0" in {
        Power(Variable("x"), 0) derive Variable("x") shouldEqual Number(0)
      }
    }
    "divide" should {
      "be (e1/e2)' = (e1*e2^(-1))'" in {
        (Variable("x") / Sin(Variable("x"))) derive Variable("x") shouldEqual (Variable("x") * (Sin(Variable("x")) ^ Number(-1))).derive(Variable("x"))
      }
      "be (number/(e^n))' = -n * number / (e^(n+1)) (e)'" in {
        (Number(10) / (Variable("x") ^ Number(3))) derive Variable("x") shouldEqual Number(-3 * 10) / (Variable("x") ^ Number(3 + 1)) * (Variable("x") derive Variable("x"))
      }
    }
    "sin" should {
      "be cos * (x)'" in {
        Sin(Variable("x") ^ Number(3)) derive Variable("x") shouldEqual Cos(Variable("x") ^ Number(3)) * (Variable("x") ^ Number(3)).derive(Variable("x"))
      }
    }
    "cos" should {
      "be -sin * (x)'" in {
        Cos(Variable("x") ^ Number(3)) derive Variable("x") shouldEqual -Sin(Variable("x") ^ Number(3)) * (Variable("x") ^ Number(3)).derive(Variable("x"))
      }
    }
  }
}
