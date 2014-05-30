package de.fosd.typechef.kconfig

import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import FeatureExprFactory._


abstract class Expr {
    //get feature expr, old binary logic version,
    // will be depricated soon
    val fexpr2: FeatureExpr

    //get string representation as written in kconfig
    def kexpr: String

    def eval(assignedValues: Set[String]): Boolean

    def eval(assignedValues: Map[String, Char]): Char

    //get expression in three-value logic, for y and m
    val fexpr_y: FeatureExpr
    val fexpr_m: FeatureExpr

    def fexpr_both: FeatureExpr = fexpr_m or fexpr_y
}

case class Name(n: Item) extends Expr {
    lazy val fexpr2: FeatureExpr = n.fexpr_y

    def kexpr = n.name

    def eval(assignedValues: Set[String]): Boolean = assignedValues.contains(n.name)

    def eval(assignedValues: Map[String, Char]): Char = assignedValues(n.name)

    lazy val fexpr_y: FeatureExpr = n.fexpr_y
    lazy val fexpr_m: FeatureExpr = if (n.isTristate) n.fexpr_m else False
}

case class And(a: Expr, b: Expr) extends Expr {
    lazy val fexpr2: FeatureExpr = a.fexpr2 and b.fexpr2

    def kexpr = a.kexpr + " && " + b.kexpr

    def eval(v: Set[String]) = a.eval(v) && b.eval(v)

    def eval(v: Map[String, Char]): Char = {
        val av = a.eval(v)
        val bv = b.eval(v)
        if (av == 'n' || bv == 'n') 'n'
        else if (av == 'm' || bv == 'm') 'm'
        else 'y'
    }

    lazy val fexpr_y: FeatureExpr = a.fexpr_y and b.fexpr_y
    lazy val fexpr_m: FeatureExpr = (a.fexpr_y or a.fexpr_m) and (b.fexpr_y or b.fexpr_m) andNot (a.fexpr_y and b.fexpr_y)
}

case class Or(a: Expr, b: Expr) extends Expr {
    lazy val fexpr2: FeatureExpr = a.fexpr2 or b.fexpr2

    def kexpr = "(" + a.kexpr + " || " + b.kexpr + ")"

    def eval(v: Set[String]) = a.eval(v) || b.eval(v)

    def eval(v: Map[String, Char]): Char = {
        val av = a.eval(v)
        val bv = b.eval(v)
        if (av == 'y' || bv == 'y') 'y'
        else if (av == 'm' || bv == 'm') 'm'
        else 'n'
    }

    lazy val fexpr_y: FeatureExpr = a.fexpr_y or b.fexpr_y
    lazy val fexpr_m: FeatureExpr = (a.fexpr_m or b.fexpr_m) and a.fexpr_y.not and b.fexpr_y.not
}

case class Not(a: Expr) extends Expr {
    lazy val fexpr2: FeatureExpr = a.fexpr2.not

    def kexpr = "!(" + a.kexpr + ")"

    def eval(v: Set[String]) = !a.eval(v)

    def eval(v: Map[String, Char]): Char = {
        val av = a.eval(v)
        if (av == 'y') 'n'
        else if (av == 'n') 'y'
        else 'm'
    }

    lazy val fexpr_y: FeatureExpr = (a.fexpr_y or a.fexpr_m).not
    lazy val fexpr_m: FeatureExpr = a.fexpr_m
}

case class YTrue() extends Expr {
    lazy val fexpr2: FeatureExpr = True

    def kexpr = "y"

    def eval(v: Set[String]) = true

    def eval(v: Map[String, Char]): Char = 'y'

    lazy val fexpr_y: FeatureExpr = True
    lazy val fexpr_m: FeatureExpr = False
}

case class HasValue(a: Expr, op: String, value: String) extends Expr {

    lazy val fexpr2: FeatureExpr = if (value != "n") True else False

    def kexpr = a + op + value

    def eval(v: Set[String]) = {
        val equal = a.eval(v) ^ (v != "n")
        if (op == "!=") !equal else equal
    }

    def eval(v: Map[String, Char]): Char = if (a.eval(v) == value.head) 'y' else 'n'

    lazy val fexpr_y: FeatureExpr = {
        val equal = value match {
            case "y" => a.fexpr_y
            case "m" => a.fexpr_m
            case "n" => a.fexpr_both.not()
            case _ => False //TODO unsupported case
        }
        if (op == "!=") equal.not else equal
    }

    lazy val fexpr_m: FeatureExpr = False


}

case class MTrue() extends Expr {
    lazy val fexpr2: FeatureExpr = False

    def kexpr = "y"

    def eval(v: Set[String]) = true

    def eval(v: Map[String, Char]): Char = 'm'

    lazy val fexpr_y: FeatureExpr = False
    lazy val fexpr_m: FeatureExpr = True
}

//
//case class EModule() extends Expr {
//    def fexpr: FeatureExpr = FeatureExprFactory.True
//    def kexpr = "m"
//    def eval(v: Set[String]) = true
//
//    //get expression in three-value logic
//    def fexpr3(_this: Item): FeatureExpr = _this.modulefexpr
//}

object Implies {
    def apply(a: Expr, b: Expr) = Or(Not(a), b)
}