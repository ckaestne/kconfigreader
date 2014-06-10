package de.fosd.typechef.kconfig

import scala.Some
import de.fosd.typechef.featureexpr.{SingleFeatureExpr, FeatureExprFactory, FeatureExpr}
import FeatureExprFactory._

private object KConfigModel {

    val MODULES = createDefinedExternal("MODULES")

}

class KConfigModel() {

    import KConfigModel.MODULES

    val items: collection.mutable.Map[String, Item] = collection.mutable.Map()
    val choices: collection.mutable.Map[String, Choice] = collection.mutable.Map()

    def setType(itemName: String, _type: String) {
        getItem(itemName).setType(_type)
    }

    def getItem(itemName: String): Item =
        items.getOrElseUpdate(itemName, Item(itemName, this))

    def getChoice(choiceName: String): Choice =
        choices.getOrElseUpdate(choiceName, Choice(choiceName))

    override def toString() = items.toString() + "\n" + choices.toString()

    def getConstraints: List[FeatureExpr] =
        (items.values.flatMap(_.getConstraints) ++
            choices.values.flatMap(_.getConstraints)).toList ++
            (if (!items.contains(MODULES.feature)) List(MODULES.not) else Nil) //if no MODULES item, exclude it explicitly

    def getFM: FeatureExpr = {


        val fm = getConstraints.foldLeft(True)(_ and _)
        if (fm.isContradiction()) {
            var f: FeatureExpr = True
            var d: String = ""
            for (c <- getConstraints) {
                assert((f and c).isSatisfiable(), "model is unsatisfiable, because " + c + ", before \n" + d)
                d = d + "\n"+c
                f = f and c
            }
        }
        fm
    }

    def getItems = items.keys.toSet

    def getBooleanSymbols: Set[SingleFeatureExpr] = {
        val i = items.values.filterNot(_.isChoice)
        val boolitems = i.filter(_._type == "boolean")
        val triitems = i.filter(_._type == "tristate")
        (boolitems.map(_.name) ++ triitems.map(_.name) ++ triitems.map(_.name + "_MODULE")).toSet.map(FeatureExprFactory.createDefinedExternal)
    }

    def getAllSymbols: Set[SingleFeatureExpr] = getBooleanSymbols ++
        (for (i <- items.values; if i.isNonBoolean; v <- i.knownValues) yield i.getNonBooleanValue(v))

    def getNonBooleanDefaults: Map[Item, List[(String, Expr)]] =
        items.values.filter(Set("integer", "hex", "string") contains _._type).map(i => (i -> i.defaultValues)).toMap

    /**
     * this is a global operation that finds possible values for all items
     * should be called after all items are known and before computing constraints
     *
     * known values come from defaults and from literals in comparisons
     */
    def findKnownValues {
        for (item <- items.values) {
            item.knownValues = item._type match {
                case "boolean" => Set("y", "n")
                case "tristate" => Set("y", "m", "n")
                case "integer" => if (item.default.isEmpty && item.hasPrompt != Not(YTrue())) Set("0") else Set("n")
                case "hex" => if (item.default.isEmpty && item.hasPrompt != Not(YTrue())) Set("0x0") else Set("n")
                case "string" => if (item.default.isEmpty && item.hasPrompt != Not(YTrue())) Set("") else Set("n")
            }
            item.knownValues ++= item.getDefaultValues
        }


        def findValues(e: Expr): Unit = e match {
            case And(a, b) => findValues(a); findValues(b)
            case Or(a, b) => findValues(a); findValues(b)
            case Not(a) => findValues(a)
            case Equals(Name(n), ConstantSymbol(v)) =>
                n.knownValues += v
            case Equals(ConstantSymbol(v), Name(n)) =>
                n.knownValues += v
            case _ =>
        }

        for (item <- items.values) {
            item.default.map(s => findValues(s._2))
            item.selectedBy.map(s => findValues(s._2))
            item.depends.map(s => findValues(s))
        }
    }
}

/**
 * tristate to CONFIG_x translation:
 *
 * x=y
 * => #define CONFIG_x
 * => #undef CONFIG_x_MODULE
 *
 * x=m
 * => #undef CONFIG_x
 * => #define CONFIG_x_MODULE
 *
 * x=n
 * => #undef CONFIG_x
 * => #undef CONFIG_x_MODULE
 *
 * @param name
 */

case class Item(val name: String, model: KConfigModel) {


    var _type: String = "boolean"
    var hasPrompt: Expr = Not(YTrue())
    private[kconfig] var default: List[(Expr /*value*/ , Expr /*visible*/ )] = Nil
    var depends: Option[Expr] = None
    var selectedBy: List[(Item, Expr)] = Nil
    var isDefined: Boolean = false
    var isChoice: Boolean = false //item is a choice? (used for filtering)
    // an item may be created because it's referenced - when it's never used it is stored as undefined

    lazy val fexpr_y = if (isNonBoolean) fexpr_nonboolean
    else FeatureExprFactory.createDefinedExternal(name)
    lazy val modulename = if (isTristate) this.name + "_MODULE" else name
    lazy val fexpr_m = if (isTristate) FeatureExprFactory.createDefinedExternal(modulename) else False
    lazy val fexpr_both = if (isTristate) (fexpr_y or fexpr_m) else fexpr_y

    def fexpr_nonboolean = knownValues.filterNot(_ == "n").map(getNonBooleanValue).foldLeft(False)(_ or _)

    var tristateChoice = false
    //special hack for choices
    var knownValues: Set[String] = Set()

    def setType(_type: String) = {
        this._type = _type

        this
    }

    def setDefined() = {
        isDefined = true
        this
    }

    def getNonBooleanValue(value: String): SingleFeatureExpr = FeatureExprFactory.createDefinedExternal(name + "=" + value)

    import KConfigModel.MODULES

    def setPrompt(p: Expr) {
        this.hasPrompt = p
    }

    def setChoice() {
        this.isChoice=true
    }

    def setDefault(defaultValue: Expr, condition: Expr) {
        this.default ::=(defaultValue, condition)
    }

    import FExprHelper._

    def setDepends(s: Expr) {

        this.depends = if (depends.isDefined) Some(Or(s, depends.get)) else Some(s)
    }

    def setSelectedBy(item: Item, condition: Expr = YTrue()) {
        this.selectedBy = (item, condition) :: this.selectedBy
    }

    def getConstraints: List[FeatureExpr] = if (isDefined || isChoice) {
        var result: List[FeatureExpr] = Nil


        //dependencies
        if (depends.isDefined) {
            if (isTristate) {
                result ::= this.fexpr_y implies depends.get.fexpr_y
                result ::= this.fexpr_m implies depends.get.fexpr_both
            } else
                result ::= this.fexpr_y implies depends.get.fexpr_both
        }

        //invisible options
        var promptCondition = hasPrompt
        if (promptCondition == this.depends.getOrElse(YTrue())) promptCondition == YTrue()
        if (promptCondition != YTrue()) {
            val nopromptCond = promptCondition.fexpr_both.not() //the if simplifies the formula in a common case. should be equivalent overall
            val defaults = getDefaults()


            val default_y = getDefault_y(defaults)
            val default_m = getDefault_m(defaults)
            val default_both = default_y or default_m
            //            println("y=" + default_y + ";m=" + default_m + ";both=" + default_both)

            //if invisible and off by default, then can only be activated by selects
            // notDefault -> !this | dep1 | dep2 | ... | depn
            if (isTristate) {
                result ::= nopromptCond implies (MODULES implies (default_y.not implies selectedBy.foldLeft(this.fexpr_y.not)((expr, sel) => (sel._1.fexpr_y and sel._2.fexpr_y) or expr)))
                result ::= nopromptCond implies (MODULES implies (default_m.not implies selectedBy.foldLeft(this.fexpr_m.not)((expr, sel) => (sel._1.fexpr_m and sel._2.fexpr_both) or expr)))
                result ::= nopromptCond implies (MODULES.not implies (default_both.not implies selectedBy.foldLeft(this.fexpr_y.not)((expr, sel) => (sel._1.fexpr_both and sel._2.fexpr_both) or expr)))
            } else if (!isNonBoolean) {
                //if _type == boolean
                result ::= nopromptCond implies ((default_both.not implies selectedBy.foldLeft(this.fexpr_y.not)((expr, sel) => (sel._1.fexpr_both and sel._2.fexpr_both) or expr)))
            } else {
                //if nonboolean
                val default_any = defaults.map(_._2).foldLeft(False)(_ or _)
                result ::= nopromptCond implies ((default_any.not implies selectedBy.foldLeft(this.fexpr_y.not)((expr, sel) => (sel._1.fexpr_both and sel._2.fexpr_both) or expr)))
            }

            //if invisible and on by default, then can only be deactivated by dependencies (== default conditions)
            // default -> this <=> defaultCondition
            if (isTristate) {
                result ::= nopromptCond implies (default_y implies this.fexpr_y)
                result ::= nopromptCond implies (default_m implies this.fexpr_both)
            } else if (!isNonBoolean) /*IF type == boolean*/ {
                var c = (default_both implies this.fexpr_y)
                //special hack for tristate choices, that are optional if modules are selected but mandatory otherwise
                if (tristateChoice) c = MODULES.not implies c
                result ::= nopromptCond implies c
            } else {
                //if isNonBoolean
                for ((defaultvalue, cond) <- defaults) {
                    assert(knownValues contains defaultvalue)
                    val f = getNonBooleanValue(defaultvalue)
                    result ::= nopromptCond implies (cond implies f)
                }
            }
        }

        if (isTristate) {
            result ::= (this.fexpr_y and this.fexpr_m).not
            result ::= (this.fexpr_m implies MODULES)
        }


        //selected by any select-dependencies
        // -> (dep1 | dep2 | ... | depn) -> this
        for (sel <- selectedBy) {
            result ::= ((sel._1.fexpr_y and sel._2.fexpr_y) implies this.fexpr_y)
            result ::= ((sel._1.fexpr_both and sel._2.fexpr_both) implies this.fexpr_both)
        }

        //in nonboolean options, we create one feature per known value
        //all these are disjunct and one needs to be selected
        if (isNonBoolean) {
            val values = knownValues.map(getNonBooleanValue).toList
            result ::= atLeastOne(values)
            result ++= atMostOneList(values)
        }

        //nonboolean features cannot be "n" if there is a prompt
        if (isNonBoolean && (knownValues contains "n")) {
            result ::= promptCondition.fexpr_both implies getNonBooleanValue("n").not
        }

        result
    } else List(fexpr_both.not())

    //    else {
    //        var result: List[FeatureExpr] = Nil
    //
    //
    //
    //
    //
    //        result
    //    }

    /**
     * do not rely on this, just an approximation
     * @return
     */
    def defaultValues: List[(String, Expr)] = {
        var result: List[(String, Expr)] = Nil


        for ((v, expr) <- default.reverse) {
            v match {
                case ConstantSymbol("y") if isTristate =>
                    result ::=("y", And(YTrue(), expr))
                    result ::=("m", And(MTrue(), expr))
                case ConstantSymbol(s) =>
                    result ::=(s, expr)
                case e if isTristate /*any expression is evaluated to y/n/m*/ =>
                    result ::=("y", And(YTrue(), And(v, expr)))
                    result ::=("m", And(MTrue(), And(v, expr)))
                case e /*any expression is evaluated to y/n/m*/ =>
                    result ::=("y", And(v, expr))
            }
        }
        result
    }


    /**
     * returns the different defaults and their respective conditions
     *
     * this evaluates conditions to booleans(!), i.e., m and y are both considered true
     *
     * note that his behaves different for booleans and nonbooleans. for nonbooleans
     * the default is typically a constantsymbol whereas for booleans it is
     * y or n (or m)
     */
    def getDefaults(): Map[String, FeatureExpr] = {
        var result: Map[String, FeatureExpr] = Map()
        var covered: FeatureExpr = False

        def updateResult(v: String, newCond: FeatureExpr) {
            val prevCondition = result.getOrElse(v, False)
            val cond = prevCondition or (newCond andNot covered)
            result += (v -> cond)
            covered = covered or newCond
        }

        def addDefaults(defaults: List[(Expr,Expr)], ctx: Expr) {
            for ((v, e) <- defaults) {
                val expr = if (ctx==null) e else And(e,ctx)
                v match {
                    case ConstantSymbol("y") if isTristate =>
                        updateResult("y", expr.fexpr_y)
                        updateResult("m", expr.fexpr_m)
                    case ConstantSymbol(s) =>
                        updateResult(s, expr.fexpr_both)
                    case e if isTristate /*any expression is evaluated to y/n/m*/ =>
                        updateResult("y", And(v, expr).fexpr_y)
                        updateResult("m", And(v, expr).fexpr_m)
                    case Name(i) if isNonBoolean /*reference to another nonboolean item*/=>
                        addDefaults(i.default, expr)
                    case e /*any expression is evaluated to y/n/m*/ =>
                        updateResult("y", And(v, expr).fexpr_both)
                }
            }
        }

        addDefaults(default.reverse, null)

        result
    }

    def getDefaultValues(): Set[String] = {
        var result: Set[String] = Set()

        for ((v, expr) <- default.reverse) {
            v match {
                case ConstantSymbol("y") if isTristate =>
                    result += "y"
                    result += "m"
                case ConstantSymbol(s) =>
                    result += s
                case e if isTristate /*any expression is evaluated to y/n/m*/ =>
                    result += "y"
                    result += "m"
                case Name(i) if isNonBoolean /*reference to another nonboolean item*/=>
                    result ++= i.getDefaultValues()
                case e /*any expression is evaluated to y/n/m*/ =>
                    result += "y"
            }
        }
        result
    }


    def getDefault_y(defaults: Map[String, FeatureExpr]) =
        defaults.filterKeys(model.items.contains(_)).map(
            e => model.getItem(e._1).fexpr_y and e._2
        ).foldLeft(
                defaults.getOrElse("y", False))(
                _ or _
            )

    def getDefault_m(defaults: Map[String, FeatureExpr]) =
        defaults.filterKeys(model.items.contains(_)).map(
            e => model.getItem(e._1).fexpr_m and e._2
        ).foldLeft(
                defaults.getOrElse("m", False))(
                _ or _
            )


    def isTristate = _type == "tristate"

    def isNonBoolean = !(Set("boolean", "tristate") contains _type)


    override def toString = "Item " + name
}


case class Choice(val name: String) {
    var required: String = ""
    var _type: String = "boolean"
    var items: List[Item] = Nil
    lazy val fexpr_y = FeatureExprFactory.createDefinedExternal(name)
    lazy val fexpr_m = if (isTristate) FeatureExprFactory.createDefinedExternal(name + "_MODULE") else False
    lazy val fexpr_both = fexpr_m or fexpr_y

    import KConfigModel.MODULES

    def setType(_type: String) = {
        this._type = _type
        this
    }

    def setRequired(p: String) = {
        this.required = p
        this
    }

    def addItem(p: Item) = {
        this.items = p :: this.items
        this
    }

    def getConstraints: List[FeatureExpr] = {
        var result: List[FeatureExpr] = List()
        //whether choices are mandatory or depend on others are set by the Items abstraction, not here
        //choice -> at least one child
        result ::= (this.fexpr_both implies (items.foldLeft(False)(_ or _.fexpr_both)))
        //every option implies the choice
        result ++= items.map(_.fexpr_both implies this.fexpr_both)
        //children can only select "m" if entire choice is "m"
        if (isTristate)
            result ++= items.filter(_.isTristate).map(_.fexpr_m implies this.fexpr_m)
        //all options are mutually exclusive in "y" setting (not in "m")
        result ++=
            (for (a <- items.tails.take(items.size); b <- a.tail) yield (a.head.fexpr_y mex b.fexpr_y))
        //if one entry is selected as "y" no other entry may be selected as "m"
        if (isTristate)
            result ++= (for (a <- items) yield
                a.fexpr_y implies items.foldLeft(True)((f, i) => f and i.fexpr_m.not()))

        if (isTristate) {
            result ::= (fexpr_m mex fexpr_y)
            result ::= (fexpr_m implies MODULES)
        }

        //this is mandatory
        result
    }

    def isTristate = _type == "tristate"
}

object FExprHelper {
    def oneOf(features: List[FeatureExpr]): FeatureExpr =
        atLeastOne(features) and atMostOne(features)

    def atLeastOne(featuresNames: List[FeatureExpr]): FeatureExpr =
        featuresNames.foldLeft(False)(_ or _)

    def atMostOne(features: List[FeatureExpr]): FeatureExpr =
        (for ((a, b) <- pairs(features)) yield a mex b).
            foldLeft(True)(_ and _)

    def atMostOneList(features: List[FeatureExpr]): List[FeatureExpr] =
        (for ((a, b) <- pairs(features)) yield a mex b).toList


    def pairs[A](elem: List[A]): Iterator[(A, A)] =
        for (a <- elem.tails.take(elem.size); b <- a.tail) yield (a.head, b)

}