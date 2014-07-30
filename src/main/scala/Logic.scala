package de.fosd.typechef.kconfig

import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import FeatureExprFactory._

/**
 * Expr represents expressions that support 3-value logic and nonboolean constraints.
 *
 * Expr supports a sound translation into propositional formula through fexpr_y and fexpr_m.
 * fexpr_y describes a boolean constraint when this expression evaluates to 'y' and fexpr_m describes
 * a boolean constraint when this expression evaluates to 'm'. 3-value logic is properly encoded through
 * the operations in subclasses, for example, negation negates fexpr_y but not fexpr_m.
 * Nonboolean options are encoded with all known values and for boolean (not tristate) options
 * fexpr_m is always false.
 *
 *
 * Furthermore fexpr2 provides an unsound approximation that ignores the 'm' state.
 */
abstract class Expr {
    /**
     * unsound approximation of Expr with propositional logic. Completely ignores the
     * 3-value logic and nonboolean constraints
     *
     * Do not rely on this
     */
    val fexpr2: FeatureExpr

    /**
     * get string representation as written in kconfig
     */
    def kexpr: String

    /**
     * evaluate the expression, assuming that the provided parameters are set to 'y'
     * and all other parameters are set to 'n'. No support for nonboolean options.
     *
     * Do not rely on this
     */
    def eval(assignedValues: Set[String]): Boolean

    /**
     * evaluate the expression, where the parameter provides the values
     * (m, n, y, literals, etc) for all options (no quotes)
     *
     * returns again a result as string (m, n, y, literal)
     */
    def eval(assignedValues: Map[String, String]): String

    /**
     * boolean encoding of three-value logic
     */
    val fexpr_y: FeatureExpr
    val fexpr_m: FeatureExpr

    /**
     * boolean encoding of whether this is activated at all (m or y)
     */
    def fexpr_both: FeatureExpr = fexpr_m or fexpr_y
}

/**
 * a symbol is either a constant literal or a reference to another item
 */
sealed trait Symbol extends Expr {
    val fexpr2: FeatureExpr

    def kexpr: String

    def eval(assignedValues: Set[String]): Boolean

    def eval(assignedValues: Map[String, String]): String
}

/**
 * constant literal with value v
 *
 * boolean encoding as true only if "m" or "y"
 */
case class ConstantSymbol(v: String) extends Expr with Symbol {

    override def kexpr: String = "'" + v + "'"

    def eval(assignedValues: Set[String]): Boolean = v == "y"

    def eval(assignedValues: Map[String, String]): String = v

    lazy val fexpr2: FeatureExpr = if (v == "y") True else False
    override val fexpr_y: FeatureExpr = if (v == "y") True else False
    override val fexpr_m: FeatureExpr = if (v == "m") True else False
}

/**
 * reference to an item, using their values for fexpr_m (tristate only) and fexpr_y
 */
case class Name(n: Item) extends Expr with Symbol {
    lazy val fexpr2: FeatureExpr = n.fexpr_y

    def kexpr = n.name

    //true if item name contained in set of assigned values
    def eval(assignedValues: Set[String]): Boolean = assignedValues.contains(n.name)

    //simply lookup the value
    def eval(assignedValues: Map[String, String]): String =
        if (assignedValues.contains(n.name))
            assignedValues(n.name)
        else throw new KConfigModelException("value for option %s not provided".format(n.name))

    lazy val fexpr_y: FeatureExpr = n.fexpr_y
    //only tristate features can have a satisfiable constraint for fexpr_m
    lazy val fexpr_m: FeatureExpr = if (n.isTristate) n.fexpr_m else False

}

/**
 * conjunction of two Expr
 */
case class And(a: Expr, b: Expr) extends Expr {
    lazy val fexpr2: FeatureExpr = a.fexpr2 and b.fexpr2

    def kexpr = a.kexpr + " && " + b.kexpr

    def eval(v: Set[String]) = a.eval(v) && b.eval(v)

    /**
     * evaluation rule for three-value logic, as defined by kconfig
     */
    def eval(v: Map[String, String]): String = {
        val av = a.eval(v)
        val bv = b.eval(v)
        if (av == "n" || bv == "n") "n"
        else if (av == "m" || bv == "m") "m"
        else "y"
    }

    //encoding of the evaluation rules with boolean options, only 'y' if both 'y'
    lazy val fexpr_y: FeatureExpr = a.fexpr_y and b.fexpr_y
    //only selected as module if one is 'm' and the other is 'm' or 'y'
    lazy val fexpr_m: FeatureExpr = (a.fexpr_y or a.fexpr_m) and (b.fexpr_y or b.fexpr_m) andNot (a.fexpr_y and b.fexpr_y)
}

/**
 * Or of expr (in 3value logic)
 */
case class Or(a: Expr, b: Expr) extends Expr {
    lazy val fexpr2: FeatureExpr = a.fexpr2 or b.fexpr2

    def kexpr = "(" + a.kexpr + " || " + b.kexpr + ")"

    def eval(v: Set[String]) = a.eval(v) || b.eval(v)

    /**
     * evaluation rule for three-value logic, as defined by kconfig
     */
    def eval(v: Map[String, String]): String = {
        val av = a.eval(v)
        val bv = b.eval(v)
        if (av == "y" || bv == "y") "y"
        else if (av == "m" || bv == "m") "m"
        else "n"
    }

    //encoding of the evaluation rules with boolean options, 'y' if either expr yields 'y'
    lazy val fexpr_y: FeatureExpr = a.fexpr_y or b.fexpr_y
    //'m' if either is 'm' and neither is 'y'
    lazy val fexpr_m: FeatureExpr = (a.fexpr_m or b.fexpr_m) and a.fexpr_y.not and b.fexpr_y.not
}

/**
 * negation of Expr
 */
case class Not(a: Expr) extends Expr {
    lazy val fexpr2: FeatureExpr = a.fexpr2.not

    def kexpr = "!(" + a.kexpr + ")"

    def eval(v: Set[String]) = !a.eval(v)

    /**
     * evaluation rule for three-value logic, as defined by kconfig:
     * negation has no effect on m
     */
    def eval(v: Map[String, String]): String = {
        val av = a.eval(v)
        if (av == "y") "n"
        else if (av == "n") "y"
        else "m"
        //TODO check behavior of negating values other than m/n/y
    }

    //boolean encoding of 3-value logic
    lazy val fexpr_y: FeatureExpr = (a.fexpr_y or a.fexpr_m).not
    lazy val fexpr_m: FeatureExpr = a.fexpr_m
}




/**
 * equals expression between two symbols (constants or item references)
 *
 * yield 'y' or 'n' depending on equality
 */
case class Equals(a: Symbol, b: Symbol) extends Expr {

    lazy val fexpr2: FeatureExpr = a.fexpr2 equiv b.fexpr2

    def kexpr = a.kexpr + "=" + b.kexpr

    def eval(v: Set[String]) = a.eval(v) == b.eval(v)


    def eval(v: Map[String, String]): String = if (a.eval(v) == b.eval(v)) "y" else "n"

    /**
     * translation of comparison to boolean formulas
     *
     * encoding is quite complicated due to different constants and boolean and nonboolean options
     */
    lazy val fexpr_y: FeatureExpr = {

        def isbool(a: Symbol) = a match {
            case ConstantSymbol(s) if !(Set("y", "m", "n") contains s) => false
            case Name(i) if (i.isNonBoolean) => false
            case _ => true
        }

        def compareNonboolConst(item: Item, value: String): FeatureExpr = {
            if (!(item.knownValues contains value))
                item.knownValues += value
            item.getNonBooleanValue(value)
        }

        /**
         * for all values that are possible in both items, create disjunction of pairs
         * (e.g., A.1=B.1 or A.2=B.2...)
         */
        def compareNonboolItems(item1: Item, item2: Item): FeatureExpr = {
            val sharedValues = item1.knownValues intersect item2.knownValues
            val pairs = for (sharedValue <- sharedValues) yield
                item1.getNonBooleanValue(sharedValue) and item2.getNonBooleanValue(sharedValue)
            pairs.foldLeft(False)(_ or _)
        }

        /**
         * equivalent comparison between two boolean/tristate features
         * supports comparison between two names or Constants that are y/m/n
         */
        def boolequiv(a: Symbol, b: Symbol) = {
            assert(isbool(a) && isbool(b))
            (a.fexpr_y equiv b.fexpr_y) and (a.fexpr_m equiv b.fexpr_m)
        }

        (a, b) match {
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
            case (aa, bb) => throw new KConfigModelException("unsupported combination: " + aa + "=" + bb)
        }
    }

    //comparisons never return 'm'
    lazy val fexpr_m: FeatureExpr = False


}

/**
 * shorthand to write Implies relationships A => B = (!A or B)
 */
object Implies {
    def apply(a: Expr, b: Expr) = Or(Not(a), b)
}

/**
 * helper object for 'y' constants
 */
object YTrue {
    def apply() = ConstantSymbol("y")
}

/**
 * helper object for 'm' constants
 */
object MTrue {
    def apply() = ConstantSymbol("m")
}