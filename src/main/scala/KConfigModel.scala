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
    val fexpr = FeatureExprFactory.createDefinedExternal(name)
    def setType(_type: String) {
        this._type = _type
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
    def setSelectedBy(item: Item, condition: Expr = ETrue()) {
        this.selectedBy = (item, condition) :: this.selectedBy
    }
    private val MODULES = createDefinedExternal("MODULES")
    def getConstraints: List[FeatureExpr] = if (Set("boolean", "tristate") contains _type) {
        var result: List[FeatureExpr] = Nil
        if (depends.isDefined)
            result = Implies(this,depends.get).fexpr :: result

        if (!hasPrompt) {
            val isDefault = getDefaultIsTrue().fexpr

            //if invisible and off by default, then can only be activated by selects
            // notDefault -> !this | dep1 | dep2 | ... | depn
            result = (isDefault.not implies selectedBy.foldLeft(this.fexpr.not)((expr, sel) => (sel._1.fexpr and sel._2.fexpr) or expr)) :: result

            //if invisible and on by default, then can only be deactivated by dependencies (== default conditions)
            // default -> this <=> defaultCondition
            result = (isDefault implies this.fexpr) :: result
        }

        if (isTristate) {
            result = (this.fexpr and this.modulefexpr).not :: result
            result ::= (this.modulefexpr implies MODULES)
        }


        //selected by any select-dependencies
        // -> (dep1 | dep2 | ... | depn) -> this
        for (sel <- selectedBy)
            result = ((sel._1.fexpr and sel._2.fexpr) implies this.fexpr) :: result

        result
    } else Nil

    def getDefaultIsTrue(): Expr = {
        var result: Expr = Not(ETrue())
        var covered: Expr = Not(ETrue())
        for ((v, expr) <- _default.reverse) {
            if (v == "y") {
                result = Or(result, And(expr, Not(covered)))
            }
            covered = Or(covered, expr)
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
    def modulename = this.name + "_MODULE"
    def modulefexpr = FeatureExprFactory.createDefinedExternal(modulename)

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
        val oneChild = (this.fexpr implies (items.foldLeft(False)(_ or _.fexpr)))
        //every option implies the choice
        val implyParent = items.map(_.fexpr implies this.fexpr)
        //all options are mutually exclusive //TODO special for tristate
        val mutex =
            for (a <- items.tails.take(items.size); b <- a.tail) yield (a.head.fexpr mex b.fexpr)
        //this is mandatory
        val thisMandatory = if (required == "required") fexpr else False
        (thisMandatory :: oneChild :: implyParent) ++ mutex
    }
}
