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

            if (command == "Item") {
                model.getItem(itemName).setType(substrs(2))
            } else
            if (command == "HasPrompts") {
                model.getItem(itemName).setPrompt(substrs(2))
            } else
            if (command == "Default") {
                //Depends <CurrentItem> <DefaultValue> "<Condition>"
                val defaultValue = substrs(2)
                val condition = if (substrs(3) == "\"y\"") ETrue() else parser.parseExpr(substrs(3).drop(1).dropRight(1))
                model.getItem(itemName).setDefault(defaultValue, condition)
            } else
            if (command == "Depends") {
                var str=substrs(2).drop(1).dropRight(1)
                str = str.replace("<choice>.....","y")
                str = str.replaceFirst("^CHOICE_\\d+","y")
                model.getItem(itemName).setDepends(parser.parseExpr(str))
            } else
            if (command == "ItemSelects") {
                //ItemSelects <CurrentItem> "<TargetItem>" "<Condition>"
                val targetItem = model.getItem(substrs(2).drop(1).dropRight(1))
                val condition = if (substrs(3) == "\"y\"") ETrue() else parser.parseExpr(substrs(3).drop(1).dropRight(1))
                targetItem.setSelectedBy(model.getItem(itemName), condition)
            } else
            if (command == "Choice") {
                model.getChoice(itemName).setRequired(substrs(2)).setType(substrs(2))
            } else
            if (command == "ChoiceItem") {
                model.getChoice(substrs(2)).addItem(model.getItem(itemName))
            } else if (!command.startsWith("#"))
                println(command)


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
            term ~ rep(("||" | "|" | "or") ~> dterm) ^^ {
                case a ~ bs => bs.foldLeft(a)(Or(_, _))
            }

        def term: Parser[Expr] =
            bool ~ rep(("&&" | "&" | "and") ~> term) ^^ {
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
                "y" ^^ { _ => ETrue()} |
                ID ^^ {
                    n =>
                        Name(fm.getItem(n))
                }

        def ID: Regex = "[A-Za-z0-9_]+".r
    }


}

