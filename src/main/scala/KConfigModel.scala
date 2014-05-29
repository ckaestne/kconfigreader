package de.fosd.typechef.kconfig

import scala.Some
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import FeatureExprFactory._


class KConfigModel() {
    val items: collection.mutable.Map[String, Item] = collection.mutable.Map()
    val choices: collection.mutable.Map[String, Choice] = collection.mutable.Map()
    def setType(itemName: String, _type: String) {
        getItem(itemName).setType(_type)
    }
    def getItem(itemName: String): Item =
        items.getOrElseUpdate(itemName, Item(itemName))
    def getChoice(choiceName: String): Choice =
        choices.getOrElseUpdate(choiceName, Choice(choiceName))

    override def toString() = items.toString() + "\n" + choices.toString()

    def getConstraints: List[FeatureExpr] = (items.values.flatMap(_.getConstraints) ++ choices.values.flatMap(_.getConstraints)).toList
    def getFM: FeatureExpr = {
        //        var f: FeatureExpr = True
        //        var d: String = ""
        //        for (c <- getConstraints) {
        //            assert((f and c).isSatisfiable(), "unsatisfiable because " + c + ", before \n" + d)
        //            d = d + "\n"+c
        //            f = f and c
        //        }

        val fm = getConstraints.foldLeft(True)(_ and _)
        assert(fm.isSatisfiable, "model is not satisfiable")
        fm
    }
    def getItems = items.keys.toSet
    def getBooleanSymbols: Set[String] = {
        val i = items.values.filterNot(_.name startsWith "CHOICE_")
        val boolitems = i.filter(_._type == "boolean")
        val triitems = i.filter(_._type == "tristate")
        (boolitems.map(_.name) ++ triitems.map(_.name) ++ triitems.map(_.name + "_MODULE")).toSet
    }

    def getNonBooleanDefaults: Map[Item, List[(String, Expr)]] =
        items.values.filter(Set("integer", "hex", "string") contains _._type).map(i => (i -> i.default)).toMap
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

case class Item(val name: String) {
    var _type: String = "boolean"
    var hasPrompt: Boolean = false
    private var _default: List[(String, Expr)] = Nil
    var depends: Option[Expr] = None
    var selectedBy: List[(Item, Expr)] = Nil
    lazy val fexpr_y = FeatureExprFactory.createDefinedExternal(name)
    lazy val fexpr_both = if (isTristate) (fexpr_y or fexpr_m) else fexpr_y
    def setType(_type: String) = {
        this._type = _type
        this
    }
    def setPrompt(p: String) {
        this.hasPrompt = p == "1"
    }
    def setDefault(defaultValue: String, condition: Expr) {
        this._default = (defaultValue, condition) :: this._default
    }
    def setDepends(s: Expr) {
        this.depends = Some(s)
    }
    def setSelectedBy(item: Item, condition: Expr = YTrue()) {
        this.selectedBy = (item, condition) :: this.selectedBy
    }
    private val MODULES = createDefinedExternal("MODULES")
    def getConstraints: List[FeatureExpr] = if (Set("boolean", "tristate") contains _type) {
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
        if (!hasPrompt) {
            val defaults = getDefaults()
            val default_y=defaults.getOrElse("y",False)
            val default_m=defaults.getOrElse("m",False)
            val default_both = default_y or default_m
            println("y=" + default_y + ";m=" + default_m+ ";both=" + default_both )

            //if invisible and off by default, then can only be activated by selects
            // notDefault -> !this | dep1 | dep2 | ... | depn
            //                        result = (isDefault.not implies selectedBy.foldLeft(this.fexpr.not)((expr, sel) => (sel._1.fexpr and sel._2.fexpr2) or expr)) :: result //TODO
            if (isTristate) {
                result ::= MODULES implies (default_y.not implies selectedBy.foldLeft(this.fexpr_y.not)((expr, sel) => (sel._1.fexpr_y and sel._2.fexpr_y) or expr))
                result ::= MODULES implies (default_m.not implies selectedBy.foldLeft(this.fexpr_m.not)((expr, sel) => (sel._1.fexpr_m and sel._2.fexpr_both) or expr))
                result ::= MODULES.not implies (default_both.not implies selectedBy.foldLeft(this.fexpr_y.not)((expr, sel) => (sel._1.fexpr_both and sel._2.fexpr_both) or expr))
            } else
                result ::= (default_both.not implies selectedBy.foldLeft(this.fexpr_y.not)((expr, sel) => (sel._1.fexpr_both and sel._2.fexpr_both) or expr))


            //if invisible and on by default, then can only be deactivated by dependencies (== default conditions)
            // default -> this <=> defaultCondition
            if (isTristate) {
                result ::= (default_y implies this.fexpr_y)
                result ::= (default_m implies this.fexpr_both)
            } else
                result ::= (default_both implies this.fexpr_y)
        }

        if (isTristate) {
            result ::= (this.fexpr_y and this.fexpr_m).not
            result ::= (this.fexpr_m implies MODULES)
        }


        //selected by any select-dependencies
        // -> (dep1 | dep2 | ... | depn) -> this
        for (sel <- selectedBy) {
            //            if (isTristate) {
            result ::= ((sel._1.fexpr_y and sel._2.fexpr_y) implies this.fexpr_y)
            result ::= ((sel._1.fexpr_both and sel._2.fexpr_both) implies this.fexpr_both)
            //            } else
            //                result ::= ((sel._1.fexpr_both and sel._2.fexpr_both) implies this.fexpr_both)
        }

        result
    } else Nil

    /**
     * returns the different defaults and their respective conditions
     *
     * this evaluates conditions to booleans(!), i.e., m and y are both considered true
     */
    def getDefaults(): Map[String, FeatureExpr] = {
        var result: Map[String, FeatureExpr] = Map()
        var covered: FeatureExpr = False

        def updateResult(v:String, newCond:FeatureExpr) {
            val prevCondition = result.getOrElse(v, False)
            val cond = prevCondition or (newCond andNot covered)
            result += (v -> cond)
            covered = covered or newCond
        }

        for ((v, expr) <- _default.reverse) {
            if (v=="y" && isTristate) {
                updateResult(v, expr.fexpr_y)
                updateResult("m", expr.fexpr_m)
                //            } else if (v=="m")
                //                updateResult(v, expr.fexpr_m)
            } else
                updateResult(v, expr.fexpr_both)

        }
        result
    }

    def default: List[(String, Expr)] = {
        var result = _default
        //        if (_type == "integer" && hasPrompt)
        //            result = ("0", ETrue()) :: result
        //        if (_type == "hex" && hasPrompt)
        //            result = ("0x0", ETrue()) :: result
        //        if (_type == "string" && hasPrompt)
        //            result = ("", ETrue()) :: result
        if (_type == "string")
            result = result.map(v => ("\"" + v._1 + "\"", v._2))
        result
    }
    def isTristate = _type == "tristate"
    lazy val modulename = this.name + "_MODULE"
    lazy val fexpr_m = FeatureExprFactory.createDefinedExternal(modulename)

}


case class Choice(val name: String) {
    var required: String = ""
    var _type: String = "boolean"
    var items: List[Item] = Nil
    val fexpr = FeatureExprFactory.createDefinedExternal(name)
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
        //choice -> at least one child //TODO unless optional
        val oneChild = (this.fexpr implies (items.foldLeft(False)(_ or _.fexpr_y)))
        //every option implies the choice
        val implyParent = items.map(_.fexpr_y implies this.fexpr)
        //all options are mutually exclusive //TODO special for tristate
        val mutex =
            for (a <- items.tails.take(items.size); b <- a.tail) yield (a.head.fexpr_y mex b.fexpr_y)
        //this is mandatory
        val thisMandatory = if (required == "required") fexpr else False
        (thisMandatory :: oneChild :: implyParent) ++ mutex
    }
}
