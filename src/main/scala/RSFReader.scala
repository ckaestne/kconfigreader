//package de.fosd.typechef.kconfig
//
//import java.io.File
//import util.parsing.combinator._
//import util.matching.Regex
//import scala.Some
//
///**
// * reads the output of undertaker-dumpconf files
// *
// * needs the modified version of dumpconf
// */
//class RSFReader {
//
//
//    /**
//     * reads a .rsf file produced by dumpconf into an internal representation
//     */
//    def readRSF(file: File): KConfigModel = {
//        val lines = io.Source.fromFile(file).getLines()
//
//        val model = new KConfigModel()
//
//        val parser = new ConstraintParser(model)
//
//
//
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
//
//        //special encoding for items that are choices
//        for (choice <- model.choices.values) {
//            //choices have weired interpretations of prompt constraints and the optional keyword,
//            //that can however be reencoded with the normal item interpretation:
//            //all choices are handled as if they are missing prompts and are on by default
//            //i.e., they are active unless dependencies prevent it
//            //dependencies attached to prompts are interpreted as normal dependencies instead
//            val choiceItem = model.getItem(choice.name)
//            choiceItem.tristateChoice = choice.isTristate
//            if (choiceItem.hasPrompt != Not(YTrue()))
//                choiceItem.setDependsAnd(choiceItem.hasPrompt)
//            choiceItem.setPrompt(if (choice.required == "optional") YTrue() else Not(YTrue()))
//            choiceItem.default = List((TristateConstant('y'), choiceItem.depends.getOrElse(YTrue())))
//
//        }
//
//        //initialize all known values once after parsing
//        model.findKnownValues
//        model
//    }
//
//
//    /**
//     * format "[lower upper]"
//     *
//     * currently only integer numbers are supported for lower and upper bounds,
//     * returning none if it cannot be parsed
//     */
//    def parseBounds(boundStr: String, isHex: Boolean): Option[(Int, Int)] = {
//        def convert(v: String): Int = if (isHex) Integer.parseInt(v.drop(2), 16) else v.toInt
//
//        if (boundStr == "") return None
//        if (boundStr.take(2) != "\"[") return None
//        if (boundStr.takeRight(2) != "]\"") return None
//        val s = boundStr.substring(2, boundStr.length - 2).split(" ")
//        if (s.length != 2) return None
//        try {
//            return Some((convert(s(0)), convert(s(1))))
//        } catch {
//            case e: NumberFormatException => return None
//        }
//    }
//
//    class ConstraintParser(fm: KConfigModel) extends RegexParsers {
//
//        def parseExpr(s: String): Expr = parseAll(expr, s) match {
//            case Success(r, _) => r
//            case NoSuccess(msg, _) => throw new Exception("error parsing " + s + " " + msg)
//        }
//
//        //implications
//        def expr: Parser[Expr] = dterm
//
//        //        "oneOf" ~ "(" ~> rep1sep(expr, ",") <~ ")" ^^ {
//        //            e => oneOf(e)
//        //        } | "atLeastOne" ~ "(" ~> rep1sep(expr, ",") <~ ")" ^^ {
//        //            e => atLeastOne(e)
//        //        } | "atMostOne" ~ "(" ~> rep1sep(expr, ",") <~ ")" ^^ {
//        //            e => atMostOne(e)
//        //        } | aterm
//        //
//        //    def aterm: Parser[Expr] =
//        //        bterm ~ opt(("=>" | "implies") ~> aterm) ^^ {
//        //            case a ~ b => if (b.isDefined) a implies b.get else a
//        //        }
//        //
//        //    def bterm: Parser[Expr] =
//        //        cterm ~ opt(("<=>" | "equiv") ~> bterm) ^^ {
//        //            case a ~ b => if (b.isDefined) a equiv b.get else a
//        //        }
//        //
//        //    //mutually exclusion
//        //    def cterm: Parser[Expr] =
//        //        dterm ~ opt(("<!>" | "mex") ~> cterm) ^^ {
//        //            case a ~ b => if (b.isDefined) a mex b.get else a
//        //        }
//        //
//        //    //||
//        def dterm: Parser[Expr] =
//            term ~ rep(("||") ~> dterm) ^^ {
//                case a ~ bs => bs.foldLeft(a)(Or(_, _))
//            }
//
//        def term: Parser[Expr] =
//            bool ~ rep(("&&") ~> term) ^^ {
//                case a ~ bs => bs.foldLeft(a)(And(_, _))
//            }
//
//        def bool: Parser[Expr] =
//            "!" ~> bool ^^ (Not(_)) |
//                ("(" ~> expr <~ ")") |
//                symbol ~ opt(("=" | "!=") ~ symbol) ^^ {
//                    case s ~ None => s
//                    case a ~ Some(op ~ b) =>
//                        val r = Equals(a, b)
//                        if (op == "!=") Not(r) else r
//                }
//
//        def symbol: Parser[Symbol] =
//            ("y" | "m" | "n") ^^ {
//                s => TristateConstant(s.head)
//            } |
//                ID ^^ {
//                    s =>
//                        try {
//                            s.toInt
//                            //if that's successful, it's an integer constant
//                            NonBooleanConstant(s)
//                        } catch {
//                            case e: NumberFormatException =>
//                                Name(fm.getItem(s))
//                        }
//
//                } |
//                "'" ~> anychar <~ "'" ^^ {
//                    s =>
//                    //this is a stupid hack because I cannot distinguish between 'y' and y in dumpconf
//                    //not sure if this is even possible in kconfig's internal representation at all
//                        if (Set("y", "m", "n") contains s) TristateConstant(s.head)
//                        else NonBooleanConstant(s)
//                }
//
//
//        def ID: Regex = "[A-Za-z0-9_]+".r
//
//        def anychar: Regex = "[^']*".r //any char except '
//
//        //        def Int: Regex = "^\\d+$".r
//    }
//
//
//}
//
