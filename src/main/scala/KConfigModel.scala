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

}

case class Item(val name: String) {
    var _type: String = "boolean"
    var hasPrompt: Boolean = false
    var default: List[(String, Expr)] = Nil
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
        this.default = (defaultValue, condition) :: this.default
    }
    def setDepends(s: Expr) {
        this.depends = Some(s)
    }
    def setSelectedBy(item: Item, condition: Expr = ETrue()) {
        this.selectedBy = (item, condition) :: this.selectedBy
    }
    def getConstraints: List[FeatureExpr] = if (_type != "boolean") Nil
    else {
        var result: List[FeatureExpr] = Nil
        if (depends.isDefined)
            result = (this.fexpr implies depends.get.fexpr) :: result

        if (!hasPrompt) {
            val isDefault = getDefaultIsTrue().fexpr

            //if invisible and off by default, then can only be activated by selects
            // notDefault -> !this | dep1 | dep2 | ... | depn
            result = (isDefault.not implies selectedBy.foldLeft(this.fexpr.not)((expr, sel) => (sel._1.fexpr and sel._2.fexpr) or expr)) :: result

            //if invisible and on by default, then can only be deactivated by dependencies (== default conditions)
            // default -> this <=> defaultCondition
            result = (isDefault implies this.fexpr) :: result
        }



        //selected by any select-dependencies
        // -> (dep1 | dep2 | ... | depn) -> this
        for (sel <- selectedBy)
            result = ((sel._1.fexpr and sel._2.fexpr) implies this.fexpr) :: result

        result
    }

    def getDefaultIsTrue(): Expr = {
        var result: Expr = Not(ETrue())
        var covered: Expr = Not(ETrue())
        for ((v, expr) <- default.reverse) {
            if (v == "\"y\"") {
                result = Or(result, And(expr, Not(covered)))
            }
            covered = Or(covered, expr)
        }
        result
    }

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
