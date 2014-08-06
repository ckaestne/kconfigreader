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
    def eval(assignedValues: Set[String]): Boolean = eval(s => if (assignedValues contains s) "y" else "n") == 'y'

    /**
     * evaluate the expression, where the parameter provides the values
     * (m, n, y, literals, etc) for all options (no quotes)
     */
    def eval(assignedValues: Map[String, String]): Char = eval(s =>
        if (assignedValues.contains(s))
            assignedValues(s)
        else throw new KConfigModelException("value for option %s not provided".format(s))
    )

    /**
     * evaluate an expression in 3-value logic (possible values y, m, and n)
     *
     * assignment is the lookup function to lookup values of item names
     */
    def eval(assignment: String => String): Char


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

    /**
     * returns the value of a symbol (any literal, including m, n, and y)
     *
     * (notice that getValue is different from eval; eval evaluates to y/m/n not to arbitrary values)
     */
    def getValue(assignment: String => String): String

    /**
     * returns the tristate representation of this symbol (note that this is different from
     * translating the value)
     */
    def getTristate(assignment: String => String): Char

    //    protected def value2expr(v: String): Char = if (v == "y" || v == "m") v.head else 'n'
}

/**
 * constant literal with value v
 *
 * boolean encoding as true only if "m" or "y"
 */
case class NonBooleanConstant(v: String) extends Expr with Symbol {

    override def kexpr: String = "'" + v + "'"

    /**
     * the value of a constant symbol is its literal
     */
    def getValue(assignment: String => String): String = v

    /**
     * the tristate value is always 'n' independent of the string representation
     */
    def getTristate(assignment: String => String): Char = 'n'


    /**
     * From spec: Convert the symbol into an expression. Boolean and tristate symbols
     * are simply converted into the respective expression values. All
     * other symbol types result in 'n'.
     *
     * Note that nonboolean values result only in y/m by comparing them to other nonboolean values
     **/
    def eval(assignment: String => String): Char = getTristate(assignment)


    //
    lazy val fexpr2: FeatureExpr = if (v == "y") True else False
    override val fexpr_y: FeatureExpr = if (v == "y") True else False
    override val fexpr_m: FeatureExpr = if (v == "m") True else False
}

/**
 * constant literal with value v
 *
 * boolean encoding as true only if "m" or "y"
 */
case class TristateConstant(v: Char) extends Expr with Symbol {

    override def kexpr: String = "" + v

    /**
     * the value of a constant symbol is its literal
     */
    def getValue(assignment: String => String): String = "" + v

    /**
     * the value of a constant symbol is its literal
     */
    def getTristate(assignment: String => String): Char = v


    /**
     * From spec: Convert the symbol into an expression. Boolean and tristate symbols
     * are simply converted into the respective expression values. All
     * other symbol types result in 'n'.
     *
     * Note that nonboolean values result only in y/m by comparing them to other nonboolean values
     **/
    def eval(assignment: String => String): Char = getTristate(assignment)


    //
    lazy val fexpr2: FeatureExpr = if (v == 'y') True else False
    override val fexpr_y: FeatureExpr = if (v == 'y') True else False
    override val fexpr_m: FeatureExpr = if (v == 'm') True else False
}


/**
 * reference to an item, using their values for fexpr_m (tristate only) and fexpr_y
 */
case class Name(n: Item) extends Expr with Symbol {
    lazy val fexpr2: FeatureExpr = n.fexpr_y

    def kexpr = n.name

    /**
     * the value of an item is it's assigned value
     */
    def getValue(assignment: String => String): String = assignment(n.name)

    //simply lookup the value
    def eval(assignment: String => String): Char = getTristate(assignment)

    def getTristate(assignment: String => String): Char =
        if (n.isNonBoolean) 'n'
        else {
            assert(Set("n", "m", "y") contains assignment(n.name), "invalid value '%s' assigned for %s".format(assignment(n.name), n.name))
            assignment(n.name).head
        }

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


    /**
     * evaluation rule for three-value logic, as defined by kconfig
     */
    def eval(v: String => String): Char = {
        val av = a.eval(v)
        val bv = b.eval(v)
        if (av == 'n' || bv == 'n') 'n'
        else if (av == 'm' || bv == 'm') 'm'
        else 'y'
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

    /**
     * evaluation rule for three-value logic, as defined by kconfig
     */
    def eval(v: String => String): Char = {
        val av = a.eval(v)
        val bv = b.eval(v)
        if (av == 'y' || bv == 'y') 'y'
        else if (av == 'm' || bv == 'm') 'm'
        else 'n'
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

    /**
     * evaluation rule for three-value logic, as defined by kconfig:
     * negation has no effect on m
     */
    def eval(v: String => String): Char = {
        val av = a.eval(v)
        if (av == 'y') 'n'
        else if (av == 'm') 'm'
        else 'y'
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

    def eval(v: String => String): Char = if (a.getValue(v) == b.getValue(v)) 'y' else 'n'

    /**
     * translation of comparison to boolean formulas
     *
     * encoding is quite complicated due to different constants and boolean and nonboolean options
     */
    lazy val fexpr_y: FeatureExpr = {

        def isbool(a: Symbol) = a match {
            case TristateConstant(_) => true
            case NonBooleanConstant(_) => false
            case Name(i) if (i.isNonBoolean) => false
            case _ => true
        }

        def compareNonboolConst(item: Item, value: String): FeatureExpr = {
            if (!(item.knownNonBooleanValues contains value))
                item.knownNonBooleanValues += value
            if (value == "") item.getNonBooleanValue(value) or item.getNonBooleanValue("n")
            else item.getNonBooleanValue(value)
        }

        /**
         * for all values that are possible in both items, create disjunction of pairs
         * (e.g., A.1=B.1 or A.2=B.2...)
         */
        def compareNonboolItems(item1: Item, item2: Item): FeatureExpr = {
            val sharedValues = item1.knownNonBooleanValues intersect item2.knownNonBooleanValues
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
            case (TristateConstant(aa), TristateConstant(bb)) => if (aa == bb) True else False
            case (NonBooleanConstant(aa), NonBooleanConstant(bb)) => if (aa == bb) True else False
            //comparing two boolean/tristate options (including y/m/n constants) is well established
            case (aa, bb) if isbool(a) && isbool(b) => boolequiv(aa, bb)
            //comparing nonboolean options actually requires a deeper comparison
            //comparing variable against constant
            case (aa@Name(aai), NonBooleanConstant(bb)) if !isbool(aa) => compareNonboolConst(aai, bb)
            case (NonBooleanConstant(bb), aa@Name(aai)) if !isbool(aa) => compareNonboolConst(aai, bb)
            //comparing nonboolean items with tristate constants will never return true
            case (aa@Name(aai), TristateConstant(bb)) if !isbool(aa) => System.err.println("Warning: %s='%s' will always evaluate to false".format(aai.name, bb)); False
            case (TristateConstant(bb), aa@Name(aai)) if !isbool(aa) => System.err.println("Warning: '%s'=%s will always evaluate to false".format(bb, aai.name)); False
            //comparing two nonboolean variables (true if they have the same value)
            case (aa@Name(aai), bb@Name(bbi)) if !isbool(aa) && !isbool(bb) => compareNonboolItems(aai, bbi)
            //comparing a boolean item to any nonboolean constant will always return false
            case (bb@NonBooleanConstant(s), aa@Name(n)) if isbool(aa) && !isbool(bb) => System.err.println("Warning: '%s'=%s will always evaluate to false".format(s, n.name)); False
            case (aa@Name(n), bb@NonBooleanConstant(s)) if isbool(aa) && !isbool(bb) => System.err.println("Warning: %s='%s' will always evaluate to false".format(n.name, s)); False
            //more useless comparisons
            case (NonBooleanConstant(a), TristateConstant(b)) => System.err.println("Warning: '%s'=%s will always evaluate to false".format(a, b)); False
            case (TristateConstant(b), NonBooleanConstant(a)) => System.err.println("Warning: %s='%s' will always evaluate to false".format(b, a)); False
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
    def apply() = TristateConstant('y')
}

/**
 * helper object for 'm' constants
 */
object MTrue {
    def apply() = TristateConstant('m')
}