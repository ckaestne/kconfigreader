package de.fosd.typechef.kconfig

import java.io.{BufferedReader, FileReader, File}
import util.parsing.combinator._
import util.matching.Regex
import scala.Some
import scala.xml.NodeSeq

/**
 * reads the output of undertaker-dumpconf files
 *
 * needs the modified version of dumpconf
 */
class XMLDumpReader {

    val SYMBOL_OPTIONAL = 0x0100
    val SYMBOL_CHOICE = 0x0010
    var choiceID = 0
    def nextChoiceId() = {
        choiceID += 1;
        choiceID
    }

    /**
     * reads a .rsf file produced by dumpconf into an internal representation
     */
    def readRSF(file: File): KConfigModel = {
        val reader = new BufferedReader(new FileReader(file))
        //skip lines in the beginning until empty line with "." marking the start
        var line = reader.readLine()
        while (line != "." && line != null)
            line = reader.readLine()
        val xmlRoot = scala.xml.XML.load(reader)

        val model = new KConfigModel()

        val parser = new ConstraintParser(model)

        def readSubmenu(submenu: NodeSeq) {
            (submenu \ "menu") map readMenu

            (submenu \ "submenu") map readSubmenu
        }

        def readExpr(expr: NodeSeq): Expr =
            if (expr.size == 0) YTrue()
            else {
                assert(expr.size == 1)
                parser.parseExpr(expr text)
            }
        def readName(expr: NodeSeq): Option[Name] = {
            assert(expr.size == 1)
            parser.parseName(expr text)
        }
        def readList(expr: NodeSeq): List[Name] = {
            assert(expr.size == 1)
            parser.parseList(expr text)
        }
        def readRange(expr: NodeSeq): (Symbol, Symbol) = {
            assert(expr.size == 1)
            parser.parseRange(expr text)
        }

        def hasFlag(sym: NodeSeq, flag: Int): Boolean = {
            val flags = (sym \ "@flags").text.toInt
            return (flags & flag) != 0
        }

        def hasPrompt(symbol: NodeSeq): Expr = {
            val prompts = getProperty(symbol, "prompt") ++ getProperty(symbol, "menu")
            if (prompts isEmpty)
                Not(YTrue())
            else (for (prompt <- prompts) yield readExpr(prompt \ "visible" \ "expr")).reduce(Or(_, _))
        }

        def readChoice(item: Item, symbol: NodeSeq) {
            val choices = readList(getProperty(symbol, "choice") \ "expr")

            item.setChoice()
            val choice = model.getChoice(item.name)
            choice.setRequired(!hasFlag(symbol, SYMBOL_OPTIONAL))
            for (c <- choices)
                choice.addItem(c.n)
            choice.setType(symbol \ "@type" text)

            //choices. there is a corresponding item, which is marked as choice as well
            //                model.getChoice(itemName).setRequired(substrs(2)).setType(substrs(3))
            //                model.getItem(itemName).setChoice()
            //            } else
            //            if (command == "ChoiceItem") {
            //                //connect item to a choice, if it is part of a choice
            //                model.getChoice(substrs(2)).addItem(model.getItem(itemName))
            //

        }

        def getProperty(symbol: NodeSeq, proptype: String): NodeSeq =
            (symbol \ "property") filter (prop => (prop \ "@type" text) == proptype)

        def readMenu(menu: NodeSeq) {
            val symbol = menu \ "symbol"
            if ((symbol size) == 0) return;
            assert((symbol size) == 1, "%d symbols in a menu (only one expected)".format((symbol size)))

            val itemId = (symbol \ "@id" text).toInt
            var itemName = (symbol \ "name").text
            val isChoice = hasFlag(symbol, SYMBOL_CHOICE)
            assert(isChoice || itemName.size > 0, "empty non-choice symbol name")
            if (isChoice && itemName.isEmpty)
                itemName = "CHOICE_" + nextChoiceId()
            val item = model.getItem(itemId).setName(itemName)

            item.setDefined()

            //            if (!isChoice)
            item.setType(symbol \ "@type" text)

            for (prop <- getProperty(symbol, "default"))
                item.setDefault(readExpr(prop \ "expr"), readExpr(prop \ "visible" \ "expr"))
            for (prop <- getProperty(symbol, "select"))
                readName(prop \ "expr").map(_.n.setSelectedBy(item, readExpr(prop \ "visible" \ "expr")))
            for (prop <- getProperty(symbol, "range")) {
                val range = readRange(prop \ "expr")//parseBounds(, item.isHex)
                item.addRange(range._1, range._2, readExpr(prop \ "visible" \ "expr"))
            }


            item.setPrompt(hasPrompt(symbol))

            var hasProp = false
            //dependency in newer versions of kconfig is defined in a symbol property's visibility,
            //in older versions through a <dep> tag on the menu
            for (prop <- getProperty(symbol, "symbol")) {
                val dep = prop \ "visible" \ "expr"
                if (!dep.isEmpty) {
                    item.setDepends(readExpr(dep))
                    hasProp = true
                }
            }
            if (!hasProp)
                for (dep <- menu \ "dep") {
                    item.setDepends(readExpr(dep))
                    hasProp = true
                }
            if (!hasProp)
                item.setDepends(YTrue())



            //choices
            if (isChoice)
                readChoice(item, symbol)

        }


        readSubmenu(xmlRoot)

        println(model)

        //        for (line <- lines) {
        //            val substrs = line.split("\t").toList
        //            val command = substrs(0)
        //            val itemName: String = substrs.applyOrElse[Int, String](1, _ => "")
        //
        //            def cleanParseExpr(s: String): Expr = {
        //                //drop quotes
        //                assert(s.head == '"' && s.last == '"', "quotes expected around expression")
        //                var str = s.drop(1).dropRight(1)
        //                //undertaker creates some strange output when inside choices
        //                val parentChoice = model.choices.values.find(_.items contains model.getItem(itemName))
        //                if (parentChoice.isDefined) {
        //                    assert(!str.contains("<choice>....."), "unsupported old undertaker-dumpconf format")
        //                    //
        //                    str = str.replaceFirst("^CHOICE_\\d+", "y")
        //                    //dumpconf prints <choice> in some cases when it refers to the outer choice item
        //                    //this should actually be preserved because it makes a difference in tristate choices
        //                    str = str.replace("<choice>=y", parentChoice.get.name + "=y")
        //                }
        //                //CHOICE_0 is irrelevant noise
        //                if (str.endsWith(" && CHOICE_0"))
        //                    str = str.dropRight(12)
        //                parser.parseExpr(str)
        //            }
        //
        //            //parse .rsf format by looking at the initial keyword
        //            if (command == "Item") {
        //                //items are initialized and have a type
        //                model.getItem(itemName).setDefined().setType(substrs(2))
        //            } else
        //            if (command == "HasPrompts") {
        //                //ignore, just counts the number of prompts which is meaningless
        //            } else
        //            if (command == "Prompt") {
        //                //determine under which condition there is a prompt (may be optional prompt)
        //                val condition = cleanParseExpr(substrs(2))
        //                model.getItem(itemName).setPrompt(condition)
        //            } else
        //            if (command == "Default") {
        //                //one of possibly many default declarations; only the first default is selected
        //                //Depends <CurrentItem> "<DefaultValue>" "<Condition>"
        //                var defaultValue = parser.parseExpr(substrs(2).drop(1).dropRight(1))
        //                val condition = parser.parseExpr(substrs(3).drop(1).dropRight(1))
        //                model.getItem(itemName).setDefault(defaultValue, condition)
        //            } else
        //            if (command == "Depends") {
        //                //dependency expressions (includes many internal kconfig mechanism that are internally translated into depends)
        //                var expr = cleanParseExpr(substrs(2))
        //                if (itemName != "MODULES") //hack for the nesting test cases. assume modules is never dependent
        //                    model.getItem(itemName).setDepends(expr)
        //            } else
        //            if (command == "ItemSelects") {
        //                //select expressions in kconfig
        //                //ItemSelects <CurrentItem> "<TargetItem>" "<Condition>"
        //                val targetItem = model.getItem(substrs(2).drop(1).dropRight(1))
        //                val condition = if (substrs(3) == "\"y\"") YTrue() else parser.parseExpr(substrs(3).drop(1).dropRight(1))
        //                targetItem.setSelectedBy(model.getItem(itemName), condition)
        //            } else
        //            if (command == "Choice") {
        //                //choices. there is a corresponding item, which is marked as choice as well
        //                model.getChoice(itemName).setRequired(substrs(2)).setType(substrs(3))
        //                model.getItem(itemName).setChoice()
        //            } else
        //            if (command == "ChoiceItem") {
        //                //connect item to a choice, if it is part of a choice
        //                model.getChoice(substrs(2)).addItem(model.getItem(itemName))
        //            } else
        //            if (command == "Range") {
        //                //range constraints for hex and int
        //                var bounds = parseBounds(substrs(2), model.getItem(itemName).isHex)
        //                if (!bounds.isDefined)
        //                    System.err.println("warning: unsupported range (dynamic limits not supported): " + line)
        //                else {
        //                    var expr = cleanParseExpr(substrs(3))
        //                    model.getItem(itemName).addRange(bounds.get._1, bounds.get._2, expr)
        //                }
        //            } else if (!command.startsWith("#")) // comments
        //                println(command)
        //
        //
        //        }

        //special encoding for items that are choices
        for (choice <- model.choices.values) {
            //choices have weired interpretations of prompt constraints and the optional keyword,
            //that can however be reencoded with the normal item interpretation:
            //all choices are handled as if they are missing prompts and are on by default
            //i.e., they are active unless dependencies prevent it
            //dependencies attached to prompts are interpreted as normal dependencies instead
            val choiceItem = model.findItem(choice.name)
            choiceItem.tristateChoice = choice.isTristate
            //            if (choiceItem.hasPrompt != Not(YTrue()))
            //                choiceItem.setDependsAnd(choiceItem.hasPrompt)
            //            choiceItem.setPrompt(if (!choice.required) YTrue() else Not(YTrue()))
            //            choiceItem.default = List((TristateConstant('y'), choiceItem.depends.getOrElse(YTrue())))

        }

        //initialize all known values once after parsing
        model.findKnownValues
        model
    }


//    /**
//     * format "[lower upper]"
//     *
//     * currently only integer numbers are supported for lower and upper bounds,
//     * returning none if it cannot be parsed
//     */
//    def parseBounds(range: (Symbol, Symbol), isHex: Boolean): Option[(Symbol, Symbol)] = {
//        if (!range._1.isInstanceOf[NonBooleanConstant]) return None
//        if (!range._2.isInstanceOf[NonBooleanConstant]) return None
//        def convert(v: String): Int = if (isHex) Integer.parseInt(v.drop(2), 16) else v.toInt
//
//        try {
//            return Some((
//                convert(range._1.asInstanceOf[NonBooleanConstant].v),
//                convert(range._2.asInstanceOf[NonBooleanConstant].v)))
//        } catch {
//            case e: NumberFormatException => return None
//        }
//    }

    class ConstraintParser(fm: KConfigModel) extends RegexParsers {

        /**
         * this IGNORE thing is a stupid hack that seems required for older versions of
         * kconfig. Whenever there is a dependency on `m` it also depends on an unnamed
         * item. There seems no distinguishable characteristic really, so an
         * item without a name, with flag SYMBOL_AUTO, and without SYMBOL_CHOICE will
         * be printed as IGNORE and will be removed from expressions before parsing.
         * @param sl
         * @return
         */
        def parseExpr(sl: String): Expr = {
            val s = sl.replace("m && IGNORE", "m && MODULES").replace("m || !IGNORE", "m || !MODULES")
            parseAll(expr, s) match {
                case Success(r, _) => r
                case NoSuccess(msg, _) => throw new Exception("error parsing " + s + " " + msg)
            }
        }
        def parseName(s: String): Option[Name] = parse(opt(name), s) match {
            case Success(r, _) => r
            case NoSuccess(msg, _) => throw new Exception("error parsing " + s + " " + msg)
        }
        def parseList(s: String): List[Name] = parseAll(list, s) match {
            case Success(r, _) => r
            case NoSuccess(msg, _) => throw new Exception("error parsing " + s + " " + msg)
        }
        def parseRange(s: String): (Symbol, Symbol) = parseAll(range, s) match {
            case Success(r, _) => r
            case NoSuccess(msg, _) => throw new Exception("error parsing " + s + " " + msg)
        }
        //implications
        def expr: Parser[Expr] = dterm

        //        "oneOf" ~ "(" ~> rep1sep(expr, ",") <~ ")" ^^ {
        //            e => oneOf(e)
        //        } | "atLeastOne" ~ "(" ~> rep1sep(expr, ",") <~ ")" ^^ {
        //            e => atLeastOne(e)
        //        } | "atMostOne" ~ "(" ~> rep1sep(expr, ",") <~ ")" ^^ {
        //            e => atMostOne(e)
        //        } | aterm
        //
        //    def aterm: Parser[Expr] =
        //        bterm ~ opt(("=>" | "implies") ~> aterm) ^^ {
        //            case a ~ b => if (b.isDefined) a implies b.get else a
        //        }
        //
        //    def bterm: Parser[Expr] =
        //        cterm ~ opt(("<=>" | "equiv") ~> bterm) ^^ {
        //            case a ~ b => if (b.isDefined) a equiv b.get else a
        //        }
        //
        //    //mutually exclusion
        //    def cterm: Parser[Expr] =
        //        dterm ~ opt(("<!>" | "mex") ~> cterm) ^^ {
        //            case a ~ b => if (b.isDefined) a mex b.get else a
        //        }
        //
        //    //||
        def dterm: Parser[Expr] =
            term ~ rep(("||") ~> dterm) ^^ {
                case a ~ bs => bs.foldLeft(a)(Or(_, _))
            }

        def term: Parser[Expr] =
            bool ~ rep(("&&") ~> term) ^^ {
                case a ~ bs => bs.foldLeft(a)(And(_, _))
            }

        def bool: Parser[Expr] =
            "!" ~> bool ^^ (Not(_)) |
                ("(" ~> expr <~ ")") |
                symbol ~ opt(("=" | "!=") ~ symbol) ^^ {
                    case s ~ None => s
                    case a ~ Some(op ~ b) =>
                        val r = Equals(a, b)
                        if (op == "!=") Not(r) else r
                }

        def name: Parser[Name] = "S@" ~ Integer ^^ {
            case _ ~ id => Name(fm.getItem(id.toInt))
        }

        def MODULES: Parser[Name] = "MODULES" ^^ {
            _ => Name(fm.findItem("MODULES"))
        }

        def list: Parser[List[Name]] = "(" ~> name ~ opt("^" ~> list) <~ ")" ^^ {
            case n ~ l => n :: l.getOrElse(Nil)
        }

        def range: Parser[(Symbol, Symbol)] = "[" ~> (symbol <~ ",") ~ symbol <~ "]" ^^ {
            case a ~ b => (a, b)
        }

        def symbol: Parser[Symbol] =
            ("y" | "m" | "n") ^^ {
                s => TristateConstant(s.head)
            } | name | MODULES |
                "'" ~> anychar <~ "'" ^^ {
                    s =>
                        NonBooleanConstant(s)
                }


        def ID: Regex = "[A-Za-z0-9_]+".r

        def Integer: Regex = "[0-9]+".r

        def anychar: Regex = "[^']*".r //any char except '

        //        def Int: Regex = "^\\d+$".r
    }


}

