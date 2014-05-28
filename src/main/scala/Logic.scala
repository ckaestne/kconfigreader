package de.fosd.typechef.kconfig

import java.io.File
import util.parsing.combinator._
import util.matching.Regex
import scala.Some
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import FeatureExprFactory._





abstract class Expr {
    //get feature expr
    def fexpr: FeatureExpr

    //get string representation as written in kconfig
    def kexpr: String

    def eval(assignedValues: Set[String]): Boolean
}

case class Name(n: Item) extends Expr {
    def fexpr: FeatureExpr = n.fexpr
    def kexpr = n.name
    def eval(assignedValues: Set[String]): Boolean = assignedValues.contains(n.name)
}

case class And(a: Expr, b: Expr) extends Expr {
    def fexpr: FeatureExpr = a.fexpr and b.fexpr
    def kexpr = a.kexpr +" && "+b.kexpr
    def eval(v: Set[String]) = a.eval(v) && b.eval(v)
}

case class Or(a: Expr, b: Expr) extends Expr {
    def fexpr: FeatureExpr = a.fexpr or b.fexpr
    def kexpr = "("+a.kexpr +" || "+b.kexpr+")"
    def eval(v: Set[String]) = a.eval(v) || b.eval(v)
}

case class Not(a: Expr) extends Expr {
    def fexpr: FeatureExpr = a.fexpr.not
    def kexpr = "!("+a.kexpr +")"
    def eval(v: Set[String]) = !a.eval(v)
}

case class ETrue() extends Expr {
    def fexpr: FeatureExpr = FeatureExprFactory.True
    def kexpr = "y"
    def eval(v: Set[String]) = true
}