package tk.sadbuttrue.symbolic

import scala.math._

/**
  * Created by true on 05/03/16.
  */
trait Expr {
  // TODO: Any?
  def +(that: Any): Expr = (this, that) match {
    case (n: java.lang.Number, e: Expr) => Number(n.doubleValue()) + e
    case (e: Expr, n: java.lang.Number) => Number(n.doubleValue()) + e
    case (Number(0), Number(0)) => Number(0)
    case (Number(0), e: Expr) => e
    case (e, Number(0)) => e
    case (Number(n), Number(m)) => Number(n + m)
    case (Prod(Number(a), Variable(x, xneg)), Prod(Number(b), Variable(y, yneg))) if x == y && xneg == yneg => (Number(a) + Number(b)) * Variable(x, xneg)
    case (x: Expr, y: Expr) => Sum(x, y)
  }

  def -(that: Expr): Expr = (this, that) match {
    case (Number(0), Number(0)) => Number(0)
    case (Number(0), e) => -e
    case (e, Number(0)) => e
    case (Number(n), Number(m)) => Number(n - m)
    case (e, Sub(e1, e2)) => e + (-Sub(e1, e2))
    case (Prod(Number(a), Variable(x, xneg)), Prod(Number(b), Variable(y, yneg))) if x == y && xneg == yneg => (Number(a) - Number(b)) * Variable(x, xneg)
    case (x, y) => Sub(x, y)
  }

  def unary_- : Expr = this match {
    case Number(n) => Number(-n)
    case v: Variable => v.copy(negative = !v.negative)
    case const: Constant => const.copy(negative = !const.negative)
    case Prod(e1, e2) => -e1 * e2
    case Frac(e1, e2) => -e1 / e2
    case Sum(e1, e2) => -e1 + -e2
    case Sub(e1, e2) => e2 - e1
    case p: Power => p.copy(negative = !p.negative)
    case s: Sin => s.copy(negative = !s.negative)
    case c: Cos => c.copy(negative = !c.negative)
  }

  def *(that: Expr): Expr = (this, that) match {
    case (Number(0), _) => Number(0)
    case (Number(1), e) => e
    case (_, Number(0)) => Number(0)
    case (e, Number(1)) => e
    case (Number(x), Number(y)) => Number(x * y)
    case (Variable(x, neg), Number(y)) => Number(y) * Variable(x, neg)
    case (Variable(x, neg), Constant(y, yneg)) => Constant(y, yneg) * Variable(x, neg)
    case (x, y) => Prod(x, y)
  }

  def /(that: Expr): Expr = (this, that) match {
    case (Number(0), Number(0)) => Number(Double.NaN)
    case (_, Number(0)) => sys.error("Dividing by zero")
    case (Number(0), _) => Number(0)
    case (e, Number(1)) => e
    case (x, y) => Frac(x, y)
  }

  def ^(that: Expr): Expr = (this, that) match {
    case (Number(0), Number(0)) => Number(Double.NaN)
    case (x, Number(0)) => Number(1)
    case (x, Number(y)) => Power(x, y)
  }

  //  todo deal with sin and cos (check, that internal expression depends on variable)
  def derive(v: Variable): Expr = this match {
    case Number(_) => Number(0)
    case Constant(_, _) => Number(0)
    case Variable(name, _) => if (name == v.name) Number(1) else Number(0)
    case Sum(e1, e2) => (e1 derive v) + (e2 derive v)
    case Sub(e1, e2) => (e1 derive v) - (e2 derive v)
    case Prod(e1, e2) => (e1 * (e2 derive v)) + (e2 * (e1 derive v))
    case Power(e, n, neg) => if (n == 0d) Number(0) else Number(n) * Power(e, n - 1, neg) * (e derive v)
    case Frac(Number(a), Power(e, n, neg)) => Number(-n * a) / Power(e, n + 1, neg) * (e derive v)
    case Frac(e1, e2) => (e1 * Power(e2, -1)) derive v
    case Sin(e, neg) => Cos(e, neg) * (e derive v)
    case Cos(e, neg) => -Sin(e, neg) * (e derive v)
  }
}

object Expr {
  def eval(e: Expr, env: Map[String, Expr] = Map.empty): Double = e match {
    case Number(n) => n
    case Variable(name, neg) => {
      val value = eval(env(name), env)
      negative(neg) * value
    }
    case Constant(name, neg) => {
      val value = eval(env(name), env)
      negative(neg) * value
    }
    case Sum(e1, e2) => eval(e1, env) + eval(e2, env)
    case Sub(e1, e2) => eval(e1, env) - eval(e2, env)
    case Prod(e1, e2) => eval(e1, env) * eval(e2, env)
    case Frac(e1, e2) => eval(e1, env) / eval(e2, env)
    case Power(e, n, neg) => {
      val value = pow(eval(e, env), n)
      negative(neg) * value
    }
    case Sin(e, neg) => negative(neg) * sin(eval(e, env))
    case Cos(e, neg) => negative(neg) * cos(eval(e, env))
  }

  private def negative(neg: Boolean) = {
    neg match {
      case false => 1d
      case true => -1d
    }
  }
}

case class Number(x: Double) extends Expr {
  override def toString = x.toString
}

case class Variable(name: String, negative: Boolean = false) extends Expr {
  override def toString = negative match {
    case true => "-" + name
    case false => name
  }
}

case class Constant(name: String, negative: Boolean = false) extends Expr {
  override def toString = negative match {
    case true => "-" + name
    case false => name
  }
}

case class Sum(expr1: Expr, expr2: Expr) extends Expr {
  override def toString = expr1.toString + "+" + expr2.toString
}

case class Sub(expr1: Expr, expr2: Expr) extends Expr {
  override def toString = expr1.toString + "-" + expr2.toString
}

case class Prod(expr1: Expr, expr2: Expr) extends Expr {
  override def toString = {
    def factorToString(e: Expr) = e match {
      case Sum(_, _) => "(" + e.toString + ")"
      case Sub(_, _) => "(" + e.toString + ")"
      case _ => e.toString
    }
    factorToString(expr1) + "*" + factorToString(expr2)
  }
}

case class Frac(expr1: Expr, expr2: Expr) extends Expr {
  override def toString = "(" + expr1.toString + ")/(" + expr2.toString + ")"
}

case class Power(e: Expr, n: Double, negative: Boolean = false) extends Expr {
  override def toString = negative match {
    case false => e.toString + "^" + n
    case true => "-" + e.toString + "^" + n
  }
}

case class Sin(e: Expr, negative: Boolean = false) extends Expr {
  override def toString = negative match {
    case false => "sin(" + e.toString + ")"
    case true => "-" + "sin(" + e.toString + ")"
  }
}

case class Cos(e: Expr, negative: Boolean = false) extends Expr {
  override def toString = negative match {
    case false => "cos(" + e.toString + ")"
    case true => "-" + "cos(" + e.toString + ")"
  }
}