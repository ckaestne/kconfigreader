package de.fosd.typechef.kconfig

import de.fosd.typechef.featureexpr.FeatureExprFactory._
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, SingleFeatureExpr}

private object KConfigModel {

    //global feature, assumed with a constant name
    val MODULES = createDefinedExternal("MODULES")

}

/**
  * The KConfigModel is an abstraction close to the content of the .rsf files.
  * It represents conditions as Expr statements including 3-value logic and
  * nonboolean constraints.
  *
  * The model consists of items (options). Some items can additionally be choices
  * for which additional data structures are available.
  *
  * The key feature of the model is to collect constraints as propositional
  * formulas (FeatureExpr), which is computed as parts of items and choices.
  */
class KConfigModel() {

    import de.fosd.typechef.kconfig.KConfigModel.MODULES

    val items: collection.mutable.Map[Int, Item] = collection.mutable.Map()
    val choices: collection.mutable.Map[String, Choice] = collection.mutable.Map()


    //helper function to find an item. creates a new item if the item does not exist yet
    def getItem(id: Int): Item =
        items.getOrElseUpdate(id, Item(id, this))

    def findItem(itemName: String): Item = {
        val r = items.values.find(_.name == itemName)
        assert(r.isDefined, "item %s not found".format(itemName))
        r.get
    }


    def hasItem(itemName: String): Boolean =
        items.values.exists(_.name == itemName)

    //helper function to find a choice. creates a new item if the choice does not exist yet
    def getChoice(choiceName: String): Choice =
        choices.getOrElseUpdate(choiceName, Choice(choiceName))

    /**
      * debugging only
      */
    override def toString() = items.toString() + "\n" + choices.toString()

    /**
      * get all constraints from all items and choices
      *
      * if the MODULES option is not contained, it is explicitly disabled as an additional constraint
      */
    def getConstraints: List[FeatureExpr] =
        (items.values.flatMap(_.getConstraints) ++
            choices.values.flatMap(_.getConstraints)).toList ++
            (if (!items.exists(_._2.name == MODULES.feature)) List(MODULES.not) else Nil) //if no MODULES item, exclude it explicitly


    private var cachedFM: Option[FeatureExpr] = None

    /**
      * returns the constraints as a single formula (conjunction of all constraints)
      *
      * additionally performs a sanity check that the model is actually satisfiable.
      * if it is not satisfiable, it fails with a KConfigModelException that details
      * the first constraint that causes the contradiction. this helps debugging
      * but might be slow
      */
    def getFM: FeatureExpr = {
        if (!cachedFM.isDefined) cachedFM = Some(getFM_noCache);
        cachedFM.get
    }

    def getFM_noCache: FeatureExpr = {
        val init: FeatureExpr = True
        val fm = getConstraints.foldLeft(init)(_ and _)
        if (fm.isContradiction()) {
            var f: FeatureExpr = init
            var d: String = ""
            for (c <- getConstraints) {
                if (!(f and c).isSatisfiable()) throw new KConfigModelException("model is unsatisfiable, because " + c + ", before \n" + d)
                d = d + "\n" + c
                f = f and c
            }
        }
        fm
    }

    def getItems = items.keys.toSet

    /**
      * get FeatureExpr symbols for all boolean and tristate items (thus including choices) in this model (X and X_MODULE for tristate)
      */
    def getBooleanSymbols: Set[SingleFeatureExpr] = {
        val i = items.values.filterNot(_.isChoice)
        val boolitems = i.filter(_._type == BoolType)
        val triitems = i.filter(_._type == TristateType)
        (boolitems.map(_.name) ++ triitems.map(_.name) ++ triitems.map(_.name + "_MODULE")).toSet.map(FeatureExprFactory.createDefinedExternal)
    }

    /**
      * get FeatureExpr symbols for all items, including all known values of nonboolean options
      *
      * call only after the model has been initialized fully with findKnownValues
      */
    def getAllSymbols: Set[SingleFeatureExpr] = getBooleanSymbols ++
        (for (i <- items.values; if i.isNonBoolean; v <- i.knownNonBooleanValues) yield i.getNonBooleanValue(v))

    /**
      * helper function to access all default values of all nonboolean options (and their corresponding conditions)
      */
    def getNonBooleanDefaults: Map[Item, List[(String, Expr)]] =
        items.values.filter(_.isNonBoolean).map(i => (i -> i.defaultValues)).toMap

    /**
      * this is a global operation that finds possible values for all items
      * should be called after all items are known and before computing constraints
      *
      * known values come from (1) defaults, (2) range expressions, and (3) literals in comparisons in any constraints
      * (when an option is compared to a literal). for all int and hex values 0 and 1 are possible default values
      * and for all strings "" and "nonempty" are possible default values
      *
      * should only be called from kconfigreader after all items are read
      */
    def findKnownValues {
        def symbol2values(s: Symbol): Set[String] = s match {
            case NonBooleanConstant(c) => Set(c)
            case TristateConstant(c) => Set("" + c)
            case Name(n) => n.knownNonBooleanValues
        }

        for (item <- items.values) {
            item.knownNonBooleanValues = item._type match {
                case BoolType => Set()
                case TristateType => Set()
                case IntType => Set("0", "1")
                case HexType => Set("0x0", "0x1")
                case StringType => Set("", "nonempty")
            }

            //add all default values
            item.knownNonBooleanValues ++= item.getNonBooleanDefaults()

            //add all extreme values of ranges and all possible values if ranges refer to other items (defined before)
            item.knownNonBooleanValues ++= item.ranges.flatMap(range => symbol2values(range._1) ++ symbol2values(range._2))
        }


        def findValues(e: Expr): Unit = e match {
            case And(a, b) => findValues(a); findValues(b)
            case Or(a, b) => findValues(a); findValues(b)
            case Not(a) => findValues(a)
            case Equals(Name(n), NonBooleanConstant(v)) if n.isNonBoolean =>
                n.knownNonBooleanValues += v
            case Equals(NonBooleanConstant(v), Name(n)) if n.isNonBoolean =>
                n.knownNonBooleanValues += v
            case _ =>
        }

        for (item <- items.values) {
            item.default.map(s => findValues(s._2))
            item.selectedBy.map(s => findValues(s._2))
            item.depends.map(s => findValues(s))
            findValues(item.hasPrompt)
            item.ranges.map(s => findValues(s._3))
        }
    }
}

class KConfigModelException(msg: String) extends Exception(msg)

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
  */


/**
  * Item represents an item/option or choice in the kconfig model.
  * each item has a name and type (boolean by default) and belongs to a kconfig model
  *
  * an item may have a prompt (possibly under some conditions; we don't care about
  * the text of the prompt or its alternatives, only the condition when it has a
  * prompt). the interpretation is fundamentally different if there is no
  * prompt, as the item assumes its default and can only be changed by constraints
  * (depends and select)
  *
  * items can have multiple defaults under different conditions.
  *
  * an item can depend on other items through an expression, whereas selectedBy is
  * an inverse constraint. it may have defaults and range conditions (possibly under a condition).
  *
  * some items are also choices.
  *
  * conditions are always expressed as Expr constraints supporting three-value logic and
  * nonboolean constraints. FeatureExpr constraints can be derived from them
  */
case class Item(val id: Int, model: KConfigModel) {

    import de.fosd.typechef.kconfig.FExprHelper._
    import de.fosd.typechef.kconfig.KConfigModel.MODULES

    //internal representation
    var name: String = "undef"
    var _type: ItemType = BoolType
    var hasPrompt: Expr = Not(YTrue())
    private[kconfig] var default: List[(Expr /*value*/ , Expr /*visible*/ )] = Nil
    var depends: Option[Expr] = None
    var selectedBy: List[(Item, Expr)] = Nil
    var ranges: List[(Symbol, Symbol, Expr)] = Nil
    // an item may be created because it's referenced - when it's never used it is stored as undefined
    var isDefined: Boolean = false
    //item is a choice? marker only (used for filtering):
    var isChoice: Boolean = false
    //special hack for choices:
    var tristateChoice = false

    /**
      * FeatureExpr representation of this item, for y, m, both (y or m), and n (!y)
      *
      * this is nontrivial: y is the options name or a disjunction of all values for nonbooleans
      * m is x_MODULE for tristate options or false otherwise
      */
    lazy val fexpr_y = if (isNonBoolean) False else FeatureExprFactory.createDefinedExternal(name)
    lazy val modulename = if (isTristate) this.name + "_MODULE" else name
    lazy val fexpr_m = if (isTristate) FeatureExprFactory.createDefinedExternal(modulename) else False
    lazy val fexpr_both = if (isTristate) (fexpr_y or fexpr_m) else fexpr_y

    private def fexpr_nonboolean = knownNonBooleanValues.map(getNonBooleanValue).foldLeft(False)(_ or _) //without n

    def getNonBooleanValue(value: String): SingleFeatureExpr = FeatureExprFactory.createDefinedExternal(name + "=" + value)

    /**
      * list of known nonboolean values of this item. collected by KConfigModel.findKnownValues at the
      * end of the initialization
      */
    var knownNonBooleanValues: Set[String] = Set()

    /**
      * set the type to one of 5 supported types
      */
    def setType(_type: String) = {
        this._type = _type match {
            case "boolean" => BoolType
            case "tristate" => TristateType
            case "integer" => IntType
            case "hex" => HexType
            case "string" => StringType
            case _ => throw new RuntimeException("unsupported item type " + _type)
        }
        this
    }

    def isBoolean = _type == BoolType

    def isTristate = _type == TristateType

    def isNonBoolean = _type != BoolType && _type != TristateType

    def isHex = _type == HexType

    /**
      * an item is defined if it is explicitly occuring in the kconfig model (in contrast
      * to being only referenced within a constraint)
      */
    def setDefined() = {
        isDefined = true
        this
    }


    /**
      * condition under which this item has a prompt (typically y or n, but may depend on
      * other constraints). this is a disjunction of all prompts
      */
    def setPrompt(p: Expr) {
        this.hasPrompt = p
    }

    def setName(name: String): Item = {
        this.name = name
        this
    }

    /**
      * marks this item as a choice. a choice object with the same name will exist in the
      * kconfigmodel
      */
    def setChoice() {
        this.isChoice = true
    }

    /**
      * set a default under a condition, the default can be an expression
      * other than a literal, then the default is the value of the evaluated expression
      */
    def setDefault(defaultValue: Expr, condition: Expr) {
        this.default ::=(defaultValue, condition)
    }


    /**
      * add a dependency (all dependencies are stored as one disjunction)
      */
    def setDepends(s: Expr) {
        this.depends = if (depends.isDefined) Some(Or(s, depends.get)) else Some(s)
    }

    /**
      * add a dependency as conjunction to existing dependencies (used internally only)
      */
    private[kconfig] def setDependsAnd(s: Expr) {
        this.depends = if (depends.isDefined) Some(And(s, depends.get)) else Some(s)
    }

    /**
      * add a selectedby clause
      */
    def setSelectedBy(item: Item, condition: Expr = YTrue()) {
        this.selectedBy = (item, condition) :: this.selectedBy
    }

    /**
      * if an item is inside a choice select clauses do not seem to have an
      * effect (see test Kconfig_3.5). Therefore, we simply discard them here
      */
    def selectedByOutsideChoice = if (isChoiceItem()) Nil else selectedBy

    def isChoiceItem() =
        model.choices.values.exists(_.items.contains(this))

    def addRange(lower: Symbol, upper: Symbol, condition: Expr) = {
        val newCondition = And(condition, Not(ranges.map(_._3).foldRight[Expr](Not(YTrue()))(Or(_, _))))
        ranges ::=(lower, upper, newCondition)
        this
    }


    /**
      * core function to compute all the constraints of this item
      *
      * this computation is far from trivial and depends on many details of the
      * kconfig semantics.
      *
      * modify only with great care and test cases for all supported
      * kconfig features
      *
      * for fexpr_m and fexpr_y see the documentation in Expr
      */
    def getConstraints: List[FeatureExpr] = if (isDefined || isChoice) {
        var result: List[FeatureExpr] = Nil


        /*
          selected by and dependencies are relatively straightforward.
          the main surprising issue is that select overrules dependencies (that is
          one may select something through a select, even though a dependency would
          otherwise prevent this. (this may give a warning in some kconfig tools,
          but is generally accepted and an exploited behavior)
         */

        //selected by any select-dependencies
        // => (dep1 | dep2 | ... | depn) -> this
        var selectedBy_y = selectedByOutsideChoice.map(sel => sel._1.fexpr_y and sel._2.fexpr_y).foldLeft(False)(_ or _)
        var selectedBy_both = selectedByOutsideChoice.map(sel => sel._1.fexpr_both and sel._2.fexpr_both).foldLeft(False)(_ or _)
        if (!selectedByOutsideChoice.isEmpty) {
            result ::= (selectedBy_y implies this.fexpr_y)
            result ::= (selectedBy_both implies this.fexpr_both)
        }


        //dependencies
        // => this -> (dependency | selectedby)
        if (depends.isDefined) {
            if (isTristate) {
                result ::= this.fexpr_y implies (depends.get.fexpr_y or selectedBy_y)
                result ::= this.fexpr_m implies (depends.get.fexpr_both or selectedBy_both)
            } else
                result ::= (if (isNonBoolean) this.fexpr_nonboolean else this.fexpr_y) implies (depends.get.fexpr_both or selectedBy_both)
        }

        /*
          invisible options are difficult to encode. they have defaults and those defaults only
          change by other options
         */

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
                result ::= nopromptCond implies (MODULES implies (default_y.not implies selectedByOutsideChoice.foldLeft(this.fexpr_y.not)((expr, sel) => (sel._1.fexpr_y and sel._2.fexpr_y) or expr)))
                result ::= nopromptCond implies (MODULES implies (default_m.not implies selectedByOutsideChoice.foldLeft(this.fexpr_m.not)((expr, sel) => (sel._1.fexpr_both and sel._2.fexpr_both) or expr)))
                result ::= nopromptCond implies (MODULES.not implies (default_both.not implies selectedByOutsideChoice.foldLeft(this.fexpr_y.not)((expr, sel) => (sel._1.fexpr_both and sel._2.fexpr_both) or expr)))
            } else if (!isNonBoolean) {
                //if _type == boolean
                result ::= nopromptCond implies ((default_both.not implies selectedByOutsideChoice.foldLeft(this.fexpr_y.not)((expr, sel) => (sel._1.fexpr_both and sel._2.fexpr_both) or expr)))
            } else {
                //if nonboolean
                val default_any = defaults.map(_._2).foldLeft(False)(_ or _)
                result ::= nopromptCond implies ((default_any.not implies selectedByOutsideChoice.foldLeft(this.fexpr_nonboolean.not)((expr, sel) => (sel._1.fexpr_both and sel._2.fexpr_both) or expr)))
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
                    //                    assert(knownNonBooleanValues contains defaultvalue)
                    val f = getNonBooleanValue(defaultvalue)
                    result ::= nopromptCond implies (cond implies f)
                }
            }

            //tristate elements are only selectable as y if visible as y (if selectable at all)
            if (isTristate) {
                result ::= (promptCondition.fexpr_both implies (this.fexpr_y implies promptCondition.fexpr_y))
            }
        }

        if (isTristate) {
            result ::= (this.fexpr_y and this.fexpr_m).not
            result ::= (this.fexpr_m implies MODULES)
        }



        //in nonboolean options, we create one feature per known value
        //all these are disjunct and one needs to be selected
        if (isNonBoolean) {
            val values = (knownNonBooleanValues + "n").map(getNonBooleanValue).toList
            result ::= atLeastOne(values)
            result ++= atMostOneList(values)

            //constraints for ranges
            result ++= getRangeConstraints()
        }

        //nonboolean features cannot be "n" if there is a prompt
        if (isNonBoolean) {
            result ::= promptCondition.fexpr_both implies getNonBooleanValue("n").not
        }


        //when they have a nonoptional prompt, choices need to be selected (as m or y)
        //also choices require their promptcondition if selected
        if (isChoice) {
            val choice = model.getChoice(this.name)
            if (choice.required) {
                result ::= promptCondition.fexpr_both implies this.fexpr_both
            }
            result ::= this.fexpr_both implies promptCondition.fexpr_both
        }


        result
    } else List(fexpr_both.not())


    private def getRangeConstraints(): List[FeatureExpr] = {
        def parseLong(s: String, isHex: Boolean): Option[Long] =
            try {
                if (isHex) Some(java.lang.Long.parseLong(s.drop(2), 16)) else Some(s.toLong)
            } catch {
                case e: NumberFormatException =>
                    System.err.println("Warning: cannot parse range constraint \"%s\"".format(s))
                    None
            }
        def getValues(s: Symbol): Set[(FeatureExpr, Long)] = s match {
            case NonBooleanConstant(c) =>
                val value = parseLong(c, this.isHex)
                value.map(v=>Set((True, v))).getOrElse(Set())
            case TristateConstant(c) =>
                val value = if (c == 'y') 2l else if (c == 'm') 1l else 0l
                Set((True, value))
            case Name(item) =>
                assert(item.isNonBoolean, "range dependency on boolean item not supported")
                val svalues = item.knownNonBooleanValues
                for (s <- svalues; value <- parseLong(s, item.isHex))
                    yield (item.getNonBooleanValue(s), value)
        }
        assert(this.isNonBoolean, "range constraints can only be produced for nonboolean values with integer meaning")
        var result: List[FeatureExpr] = Nil
        //force comparison on integers
        for ((lower, upper, expr) <- this.ranges)
            for (value <- knownNonBooleanValues /*without n*/ ) {
                parseLong(value, this.isHex).map(v=> {
                    for ((lowervalExpr, lowervalue) <- getValues(lower))
                        if (v < lowervalue)
                            result ::= (expr.fexpr_both and lowervalExpr) implies this.getNonBooleanValue(value).not
                    for ((uppervalExpr, uppervalue) <- getValues(upper))
                        if (v > uppervalue)
                            result ::= (expr.fexpr_both and uppervalExpr) implies this.getNonBooleanValue(value).not
                })
            }

        result
    }

    /**
      * get the default values as strings
      *
      * do not rely on this, just an approximation
      */
    def defaultValues: List[(String, Expr)] = {
        var result: List[(String, Expr)] = Nil


        for ((v, expr) <- default.reverse) {
            v match {
                case TristateConstant('y') if isTristate =>
                    result ::=("y", And(YTrue(), expr))
                    result ::=("m", And(MTrue(), expr))
                case TristateConstant(s) =>
                    result ::=("" + s, expr)
                case NonBooleanConstant(s) =>
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

        def addDefaults(defaults: List[(Expr, Expr)], ctx: Expr) {
            for ((v, e) <- defaults) {
                val expr = if (ctx == null) e else And(e, ctx)
                v match {
                    case TristateConstant('y') if isTristate =>
                        updateResult("y", expr.fexpr_y)
                        updateResult("m", expr.fexpr_m)
                    case TristateConstant(s) =>
                        updateResult("" + s, expr.fexpr_both)
                    case NonBooleanConstant(s) =>
                        updateResult(s, expr.fexpr_both)
                    case e if isTristate /*any expression is evaluated to y/n/m*/ =>
                        updateResult("y", And(v, expr).fexpr_y)
                        updateResult("m", And(v, expr).fexpr_m)
                    case Name(i) if isNonBoolean /*reference to another nonboolean item*/ =>
                        addDefaults(i.default.reverse, expr)
                    case e /*any expression is evaluated to y/n/m*/ =>
                        updateResult("y", And(v, expr).fexpr_both)
                }
            }
        }

        addDefaults(default.reverse, null)

        result
    }

    /**
      * compute the set of all possible default values
      */
    def getDefaultValues(): Set[String] = {
        var result: Set[String] = Set()

        for ((v, expr) <- default.reverse) {
            v match {
                case TristateConstant('y') if isTristate =>
                    result += "y"
                    result += "m"
                case TristateConstant(s) =>
                    result += "" + s
                case NonBooleanConstant(s) =>
                    result += s
                case e if isTristate /*any expression is evaluated to y/n/m*/ =>
                    result += "y"
                    result += "m"
                case Name(i) if isNonBoolean /*reference to another nonboolean item*/ =>
                    result ++= i.getDefaultValues()
                case e /*any expression is evaluated to y/n/m*/ =>
                    result += "y"
            }
        }
        result
    }

    /**
      * compute the set of all possible nonboolean default values
      */
    def getNonBooleanDefaults(): Set[String] = {
        for ((v, expr) <- default.reverse) yield
            v match {
                case NonBooleanConstant(s) =>
                    Set(s)
                case Name(i) if isNonBoolean /*reference to another nonboolean item*/ =>
                    i.getDefaultValues()
                case _ => Set()
            }
    }.foldRight(Set[String]())(_ ++ _)


    def getDefault_y(defaults: Map[String, FeatureExpr]) =
        defaults.filterKeys(k => model.items.exists(_._2.name == k)).map(
            e => model.findItem(e._1).fexpr_y and e._2
        ).foldLeft(
            defaults.getOrElse("y", False))(
            _ or _
        )

    def getDefault_m(defaults: Map[String, FeatureExpr]) =
        defaults.filterKeys(k => model.items.exists(_._2.name == k)).map(
            e => model.findItem(e._1).fexpr_m and e._2
        ).foldLeft(
            defaults.getOrElse("m", False))(
            _ or _
        )


    override def toString = "Item " + name
}


case class Choice(val name: String) {
    var required: Boolean = false
    var _type: ItemType = BoolType
    var items: List[Item] = Nil

    lazy val fexpr_y = FeatureExprFactory.createDefinedExternal(name)
    lazy val fexpr_m = if (isTristate) FeatureExprFactory.createDefinedExternal(name + "_MODULE") else False
    lazy val fexpr_both = fexpr_m or fexpr_y

    import de.fosd.typechef.kconfig.KConfigModel.MODULES

    def setType(_type: String) = {
        this._type = _type match {
            case "boolean" => BoolType
            case "tristate" => TristateType
            case _ => throw new RuntimeException("unsupported choice type " + _type)
        }
        this
    }

    def setRequired(p: Boolean) = {
        this.required = p
        this
    }

    def addItem(p: Item) = {
        this.items = p :: this.items
        this
    }

    //return those items that have a prompt at least sometimes
    def promptItems: List[Item] = items.filter(_.hasPrompt != Not(YTrue()))

    def getConstraints: List[FeatureExpr] = {
        var result: List[FeatureExpr] = List()

        //choice (=y) -> at least one child (=y)
        result ::= (this.fexpr_y implies (promptItems.foldLeft(False)(_ or _.fexpr_y)))
        //every option (even those without prompts) implies the choice
        result ++= items.map(_.fexpr_both implies this.fexpr_both)
        //children can only select "m" if entire choice is "m"
        if (isTristate)
            result ++= items.filter(_.isTristate).map(_.fexpr_m implies this.fexpr_m)
        //if the choice is boolean, tristate items can still only be selected as y or n, not as m
        if (!isTristate)
            result ++= items.filter(_.isTristate).map(_.fexpr_m.not)
        //all options (with prompts) are mutually exclusive in "y" setting (not in "m")
        result ++=
            (for (a <- promptItems.tails.take(promptItems.size); b <- a.tail) yield (a.head.fexpr_y mex b.fexpr_y))
        //if one entry is selected as "y" no other entry may be selected as "m" (prompt only)
        if (isTristate)
            result ++= (for (a <- promptItems) yield
                a.fexpr_y implies promptItems.foldLeft(True)((f, i) => f and i.fexpr_m.not()))

        if (isTristate) {
            result ::= (fexpr_m mex fexpr_y)
            result ::= (fexpr_m implies MODULES)
        }

        //this is mandatory
        result
    }

    def isTristate = _type == TristateType
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


sealed trait ItemType

object StringType extends ItemType

object IntType extends ItemType

object HexType extends ItemType

object BoolType extends ItemType

object TristateType extends ItemType