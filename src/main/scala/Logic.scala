package de.fosd.typechef.kconfig

import java.io.File
import util.parsing.combinator._
import util.matching.Regex
import scala.Some
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import FeatureExprFactory._





abstract class Expr {
    def fexpr: FeatureExpr
}

case class Name(n: Item) extends Expr {
    def fexpr: FeatureExpr = n.fexpr
}

case class And(a: Expr, b: Expr) extends Expr {
    def fexpr: FeatureExpr = a.fexpr and b.fexpr
}

case class Or(a: Expr, b: Expr) extends Expr {
    def fexpr: FeatureExpr = a.fexpr or b.fexpr
}

case class Not(a: Expr) extends Expr {
    def fexpr: FeatureExpr = a.fexpr.not
}

case class ETrue() extends Expr {
    def fexpr: FeatureExpr = FeatureExprFactory.True
}