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
      "be value of variable on variable" in {
        eval(Variable("x", true), Map("x" -> Number(10))) shouldEqual -10
      }
      "be value of constant on constant" in {
        eval(Constant("c"), Map("c" -> Number(10))) shouldEqual 10
      }
      "be value of sum on sum" in {
        eval(Constant("c") + Number(10), Map("c" -> Number(10))) shouldEqual 20
      }
      "be value of sub on sub" in {
        eval(Constant("c") - Number(10), Map("c" -> Number(10))) shouldEqual 0
      }
      "be value of prod on prod" in {
        eval(Constant("c") * Number(10), Map("c" -> Number(10))) shouldEqual 100
      }
      "be value of div on div" in {
        eval(Constant("c") / Number(10), Map("c" -> Number(10))) shouldEqual 1
      }
      "be value of power on power" in {
        eval(Constant("c") ^ Number(10), Map("c" -> Number(2))) shouldEqual scala.math.pow(2, 10)
      }
      "be value of sin on sin" in {
        eval(-Sin(scala.math.Pi)) shouldEqual -scala.math.sin(scala.math.Pi)
      }
      "be value of cos on cos" in {
        eval(Cos(scala.math.Pi)) shouldEqual scala.math.cos(scala.math.Pi)
      }
    }
  }
}
