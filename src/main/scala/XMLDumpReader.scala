package de.fosd.typechef.kconfig

import java.io.{BufferedReader, FileReader, File}
import util.parsing.combinator._
import util.matching.Regex
import scala.Some
import scala.xml.NodeSeq
import scala.language.postfixOps

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



        //special encoding for items that are choices
        for (choice <- model.choices.values) {
            //choices have weired interpretations of prompt constraints and the optional keyword,
            //that can however be reencoded with the normal item interpretation:
            //all choices are handled as if they are missing prompts and are on by default
            //i.e., they are active unless dependencies prevent it
            //dependencies attached to prompts are interpreted as normal dependencies instead
            val choiceItem = model.findItem(choice.name)
            choiceItem.tristateChoice = choice.isTristate

        }

        //initialize all known values once after parsing
        model.findKnownValues
        model
    }



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

        def Integer: Regex = "[0-9]+".r

        def anychar: Regex = "[^']*".r //any char except '

    }


}

