package de.fosd.typechef.kconfig

import org.junit._
import scala._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory._

/**
 * tests for the three-value logic part of the extractor
 */
class Logic3Test {

    @Test
    def test1 {
        val model = new KConfigModel()
        val a = Name(model.getItem(1).setName("A").setType("tristate"))
        val b = Name(model.getItem(2).setName("B").setType("tristate"))

        //        val f = And(a, b)
        val f =

            checkF(Not(a))
        checkF(Not(Not(a)))
        checkF(Or(a, b))
        checkF(And(a, b))
        checkF(Implies(a, b))
        checkF(And(Implies(a, b), Implies(b, a)))
    }

    def checkF(f: Expr) = {
        println("checking " + f)
        println("y: " + f.fexpr_y)
        println("m: " + f.fexpr_m)

        assert((f.fexpr_m or f.fexpr_y).isSatisfiable())

        val vars = getVariables(f)

        val assignments: List[Map[Item, String]] = getAllAssignments(vars)

        for (ass <- assignments) {
            val result = f.eval(ass.map(v => (v._1.name -> v._2)))
            println(ass + " => " + result)

            val assF = getAssignmentFormula(ass)

            if (result == 'y') {
                assert((f.fexpr_y and assF).isSatisfiable(), (f.fexpr_y and assF) + " not satisfiable")
                assert((f.fexpr_m and assF).isContradiction())
            }
            if (result == 'm') {
                assert((f.fexpr_y and assF).isContradiction())
                assert((f.fexpr_m and assF).isSatisfiable(), (f.fexpr_m and assF) + " not satisfiable")
            }
            if (result == 'n') {
                assert((f.fexpr_y and assF).isContradiction())
                assert((f.fexpr_m and assF).isContradiction())
            }


        }

    }

    def getVariables(f: Expr): Set[Item] = f match {
        case Name(n) => Set(n)
        case And(a, b) => getVariables(a) ++ getVariables(b)
        case Or(a, b) => getVariables(a) ++ getVariables(b)
        case Not(a) => getVariables(a)
        case _ => Set()
    }

    def getAllAssignments(items: Iterable[Item]): List[Map[Item, String]] = if (items.isEmpty) List(Map())
    else {
        val r = getAllAssignments(items.tail)

        r.map(_ + (items.head -> "y")) ++
            r.map(_ + (items.head -> "n")) ++
            (if (items.head.isTristate)
                r.map(_ + (items.head -> "m"))
            else Nil)
    }

    def getAssignmentFormula(ass: Map[Item, String]): FeatureExpr =
        ass.map(v =>
            if (v._2 == "n") v._1.fexpr_y.not and v._1.fexpr_m.not
            else if (v._2 == "m") v._1.fexpr_y.not and v._1.fexpr_m
            else if (v._2 == "y") v._1.fexpr_y and v._1.fexpr_m.not
            else True
        ).reduce(_ and _)
}
