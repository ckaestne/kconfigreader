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

    def eval(assignedValues: Map[String, String]): String

    //get expression in three-value logic, for y and m
    val fexpr_y: FeatureExpr
    val fexpr_m: FeatureExpr

    def fexpr_both: FeatureExpr = fexpr_m or fexpr_y
}

sealed trait Symbol extends Expr {
    val fexpr2: FeatureExpr

    def kexpr: String

    def eval(assignedValues: Set[String]): Boolean

    def eval(assignedValues: Map[String, String]): String
}

case class ConstantSymbol(v: String) extends Expr with Symbol {

    override def kexpr: String = "'" + v + "'"

    def eval(assignedValues: Set[String]): Boolean = v == "y"

    def eval(assignedValues: Map[String, String]): String = v

    lazy val fexpr2: FeatureExpr = if (v == "y") True else False
    override val fexpr_y: FeatureExpr = if (v == "y") True else False
    override val fexpr_m: FeatureExpr = if (v == "m") True else False
}

case class Name(n: Item) extends Expr with Symbol {
    lazy val fexpr2: FeatureExpr = n.fexpr_y

    def kexpr = n.name

    def eval(assignedValues: Set[String]): Boolean = assignedValues.contains(n.name)

    def eval(assignedValues: Map[String, String]): String = assignedValues(n.name)

    lazy val fexpr_y: FeatureExpr = n.fexpr_y
    lazy val fexpr_m: FeatureExpr = if (n.isTristate) n.fexpr_m else False

}

case class And(a: Expr, b: Expr) extends Expr {
    lazy val fexpr2: FeatureExpr = a.fexpr2 and b.fexpr2

    def kexpr = a.kexpr + " && " + b.kexpr

    def eval(v: Set[String]) = a.eval(v) && b.eval(v)

    def eval(v: Map[String, String]): String = {
        val av = a.eval(v)
        val bv = b.eval(v)
        if (av == "n" || bv == "n") "n"
        else if (av == "m" || bv == "m") "m"
        else "y"
    }

    lazy val fexpr_y: FeatureExpr = a.fexpr_y and b.fexpr_y
    lazy val fexpr_m: FeatureExpr = (a.fexpr_y or a.fexpr_m) and (b.fexpr_y or b.fexpr_m) andNot (a.fexpr_y and b.fexpr_y)
}

case class Or(a: Expr, b: Expr) extends Expr {
    lazy val fexpr2: FeatureExpr = a.fexpr2 or b.fexpr2

    def kexpr = "(" + a.kexpr + " || " + b.kexpr + ")"

    def eval(v: Set[String]) = a.eval(v) || b.eval(v)

    def eval(v: Map[String, String]): String = {
        val av = a.eval(v)
        val bv = b.eval(v)
        if (av == "y" || bv == "y") "y"
        else if (av == "m" || bv == "m") "m"
        else "n"
    }

    lazy val fexpr_y: FeatureExpr = a.fexpr_y or b.fexpr_y
    lazy val fexpr_m: FeatureExpr = (a.fexpr_m or b.fexpr_m) and a.fexpr_y.not and b.fexpr_y.not
}

case class Not(a: Expr) extends Expr {
    lazy val fexpr2: FeatureExpr = a.fexpr2.not

    def kexpr = "!(" + a.kexpr + ")"

    def eval(v: Set[String]) = !a.eval(v)

    def eval(v: Map[String, String]): String = {
        val av = a.eval(v)
        if (av == "y") "n"
        else if (av == "n") "y"
        else "m"
    }

    lazy val fexpr_y: FeatureExpr = (a.fexpr_y or a.fexpr_m).not
    lazy val fexpr_m: FeatureExpr = a.fexpr_m
}

object YTrue {
    def apply() = ConstantSymbol("y")
}

object MTrue {
    def apply() = ConstantSymbol("m")
}


case class Equals(a: Symbol, b: Symbol) extends Expr {

    lazy val fexpr2: FeatureExpr = a.fexpr2 equiv b.fexpr2

    def kexpr = a.kexpr + "=" + b.kexpr

    def eval(v: Set[String]) = a.eval(v) == b.eval(v)


    def eval(v: Map[String, String]): String = if (a.eval(v) == b.eval(v)) "y" else "n"


    lazy val fexpr_y: FeatureExpr = (a, b) match {
        //comparing two constants is easy
        case (ConstantSymbol(aa), ConstantSymbol(bb)) => if (aa == bb) True else False
        //comparing two boolean/tristate options (including y/m/n constants) is well established
        case (aa, bb) if isbool(a) && isbool(b) => boolequiv(aa, bb)
        //comparing nonboolean options actually requires a deeper comparison
        //comparing variable against constant
        case (aa@Name(aai), ConstantSymbol(bb)) if !isbool(aa) => compareNonboolConst(aai, bb)
        case (ConstantSymbol(bb), aa@Name(aai)) if !isbool(aa) => compareNonboolConst(aai, bb)
        //comparing two nonboolean variables (true if they have the same value)
        case (aa@Name(aai), bb@Name(bbi)) if !isbool(aa) && !isbool(bb) => compareNonboolItems(aai, bbi)
        //comparing a boolean to any string other than n/m/y will always return false
        case (bb@ConstantSymbol(s), aa@Name(n)) if isbool(aa) && !isbool(bb) => System.err.println("Warning: '%s'=%s will always evaluate to false".format(s, n.name)); False
        case (aa@Name(n), bb@ConstantSymbol(s)) if isbool(aa) && !isbool(bb) => System.err.println("Warning: %s='%s' will always evaluate to false".format(n.name, s)); False
        // unknown?
        case (aa, bb) => assert(false, "unsupported combination: " + aa + "=" + bb); False

    }

    private def isbool(a: Symbol) = a match {
        case ConstantSymbol(s) if !(Set("y", "m", "n") contains s) => false
        case Name(i) if (i.isNonBoolean) => false
        case _ => true
    }

    private def compareNonboolConst(item: Item, value: String): FeatureExpr = {
        if (!(item.knownValues contains value))
            item.knownValues += value
        item.getNonBooleanValue(value)
    }

    private def compareNonboolItems(item1: Item, item2: Item): FeatureExpr = {
        val sharedValues = item1.knownValues intersect item2.knownValues
        val pairs = for (sharedValue <- sharedValues) yield
            item1.getNonBooleanValue(sharedValue) and item2.getNonBooleanValue(sharedValue)
        pairs.foldLeft(False)(_ or _)
    }

    /**
     * equivalent comparison between two boolean/tristate features
     * supports comparison between two names or Constants that are y/m/n
     */
    private def boolequiv(a: Symbol, b: Symbol) = {
        assert(isbool(a) && isbool(b))
        (a.fexpr_y equiv b.fexpr_y) and (a.fexpr_m equiv b.fexpr_m)
    }

    lazy val fexpr_m: FeatureExpr = False


}


object Implies {
    def apply(a: Expr, b: Expr) = Or(Not(a), b)
}