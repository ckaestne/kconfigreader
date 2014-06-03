package de.fosd.typechef.kconfig

import java.io.File
import util.parsing.combinator._
import util.matching.Regex
import scala.Some
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import FeatureExprFactory._

/**
 * reads the output of undertaker dumpconf files
 */
class RSFReader {


    def readRSF(file: File): KConfigModel = {
        val lines = io.Source.fromFile(file).getLines()

        val model = new KConfigModel()

        val parser = new ConstraintParser(model)



        for (line <- lines) {
            val substrs = line.split("\t").toList
            val command = substrs(0)
            val itemName: String = substrs.applyOrElse[Int, String](1, _ => "")

            def cleanParseExpr(s: String): Expr = {
                var str = s.drop(1).dropRight(1)
                //undertaker creates some strange output when inside choices
                val parentChoice = model.choices.values.find(_.items contains model.getItem(itemName))
                if (parentChoice.isDefined) {
                    str = str.replace("<choice>.....", "y")
                    str = str.replaceFirst("^CHOICE_\\d+", "y")
                    str = str.replace("<choice>=y", parentChoice.get.name + "=y")//this should actually be preserved because it makes a difference in tristate choices
                }
                if (str.endsWith(" && CHOICE_0"))
                    str = str.dropRight(12)
                parser.parseExpr(str)
            }


            if (command == "Item") {
                model.getItem(itemName).setDefined().setType(substrs(2))
            } else
            if (command == "HasPrompts") {
                //ignore, just counts the number of prompts which is meaningless
            } else
            if (command == "Prompt") {
                val condition = cleanParseExpr(substrs(2))
                model.getItem(itemName).setPrompt(condition)
            } else
            if (command == "Default") {
                //Depends <CurrentItem> "<DefaultValue>" "<Condition>"
                var defaultValue = substrs(2).drop(1).dropRight(1)
                val condition = parser.parseExpr(substrs(3).drop(1).dropRight(1))
                model.getItem(itemName).setDefault(defaultValue, condition)
            } else
            if (command == "Depends") {
                var expr = cleanParseExpr(substrs(2))
                if (itemName!="MODULES")//hack for the nesting test cases. assume modules is never dependent
                    model.getItem(itemName).setDepends(expr)
            } else
            if (command == "ItemSelects") {
                //ItemSelects <CurrentItem> "<TargetItem>" "<Condition>"
                val targetItem = model.getItem(substrs(2).drop(1).dropRight(1))
                val condition = if (substrs(3) == "\"y\"") YTrue() else parser.parseExpr(substrs(3).drop(1).dropRight(1))
                targetItem.setSelectedBy(model.getItem(itemName), condition)
            } else
            if (command == "Choice") {
                model.getChoice(itemName).setRequired(substrs(2)).setType(substrs(3))
            } else
            if (command == "ChoiceItem") {
                model.getChoice(substrs(2)).addItem(model.getItem(itemName))
            } else if (!command.startsWith("#"))
                println(command)


        }
        for (choice <- model.choices.values) {
            //all choices are handled as if they are missing prompts and are on by default
            //i.e., they are active unless dependencies prevent it
            val choiceItem = model.getItem(choice.name)
            choiceItem.tristateChoice = choice.isTristate
            choiceItem.setPrompt(if (choice.required == "optional") YTrue() else Not(YTrue()))
            choiceItem._default = List(("y", choiceItem.depends.getOrElse(YTrue())))

        }

        model
    }

    class ConstraintParser(fm: KConfigModel) extends RegexParsers {

        def parseExpr(s: String): Expr = parseAll(expr, s) match {
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
                //            "InvalidExpression()" ^^ (_ => featureFactory.False) |
                //            (("definedEx" | "defined" | "def") ~ "(" ~> ID <~ ")") ^^ {
                //                toFeature(_)
                //            } |
                //            ("1" | "true" | "True" | "TRUE") ^^ {
                //                x => featureFactory.True
                //            } |
                //            ("0" | "false" | "False" | "FALSE") ^^ {
                //                x => featureFactory.False
                //            } |
                "y" ^^ { _ => YTrue()} |
                "n" ^^ { _ => Not(YTrue())} |
                "m" ^^ { _ => MTrue()} |
                //                Int ^^ {_ => YTrue()} | //TODO accepting numbers/strings not supported yet
                ID ~ opt(("=" | "!=") ~ opt(bool)) ^^ {
                    case n ~ v =>
                        try {
                            n.toInt
                            //if that's successful, that's not a supported ID right now
                            YTrue()
                        } catch {
                            case e: NumberFormatException =>

                                val r = Name(fm.getItem(n))
                                if (v.isDefined && v.get._2.isDefined) {
                                    //TODO handle case of empty strings properly
                                    val s = Equals(r, v.get._2.get)
                                    if (v.get._1 == "!=") Not(s) else s
                                } else r
                        }
                }

        def ID: Regex = "[A-Za-z0-9_]+".r

        def Int: Regex = "^\\d+$".r
    }


}

