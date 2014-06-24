package de.fosd.typechef.kconfig

import org.junit._
import java.io._
import de.fosd.typechef.featureexpr.{FeatureExprParser, FeatureExpr, FeatureExprFactory}
import scala._
import FeatureExprFactory._
import java.net.URI
import org.sat4j.core.{VecInt, Vec}
import org.sat4j.specs.IVecInt
import de.fosd.typechef.busybox.DimacsWriter
import de.fosd.typechef.featureexpr.sat.{SATFeatureModel, SATFeatureExpr}
import scala.io.Source

@Ignore
class LinuxTest extends DifferentialTesting {

    lazy val x86model = getModel("x86")
    lazy val x86kconfig = kconfigFile("x86")

    def kconfigFile(arch: String) = "arch/" + arch + "/Kconfig"

    val workingDir = new File(linuxTreeRoot)

    def getModel(arch: String): KConfigModel = {

        val rsfFile = new File(workingDir, arch + ".rsf")

        getModel(workingDir, kconfigFile(arch), rsfFile)
    }



    @Test
    @Ignore
    def testLoadLinux() {
        for (arch <- List("x86", "arm")) {
            println("getting model")
            val model = getModel(arch)

            println("getting constraints")
            val allconstraints = model.getConstraints

            println("checking combined constraint")
            val isSat = allconstraints.reduce(_ and _).isSatisfiable()
            if (!isSat) {
                println("checking each constraint")
                assert(allconstraints.forall(_.isSatisfiable()), "extracted constraint is not satisfiable")
            }
            assert(isSat, "extracted model is not satisfiable")
            //

            println("writing model")
            KConfigReader.writeModel(new File(workingDir, arch + ".model"), model)

            println("writing nonboolean")
            KConfigReader.writeNonBoolean(model, new File(workingDir, arch + ".nonbool.h"))

            //            println("reducing constraints for dimacs")
            //            val reducedconstraints = reduceConstraints(allconstraints)
            println("writing dimacs")
            new DimacsWriter().writeAsDimacs2(allconstraints.map(_.asInstanceOf[SATFeatureExpr]), new File(workingDir, arch + ".dimacs"))


        }
    }




    @Test
    def test32vs64() {

        genAllCombinationsFromPartial(x86kconfig, workingDir, x86model,
            Set(/*"X86_32","X86_64"*/))
    }

    @Test
    def approxfmTest() {
        //approx.fm contained several handwritten constraints that are known to hold in the linux kernel

        val fm = getModel("x86").getFM

        for (line <- Source.fromFile("approx.fm").getLines()) {
            val lineExpr = new FeatureExprParser().parse(line)

            assert((fm implies lineExpr).isTautology(), "approx.fm line '%s' was not guaranteed by feature model".format(lineExpr))
        }


    }

    def createFromDimacsFile_2Var(file: URI): SATFeatureModel = createFromDimacsFile_2Var(scala.io.Source.fromFile(file))

    def createFromDimacsFile_2Var(file: String): SATFeatureModel = createFromDimacsFile_2Var(scala.io.Source.fromFile(file))

    def createFromDimacsFile_2Var(source: scala.io.Source): SATFeatureModel = {
        var variables: Map[String, Int] = Map()
        val clauses = new Vec[IVecInt]()
        var maxId = 0

        for (line <- source.getLines) {
            if (line startsWith "c ") {
                val entries = line.substring(2).split(" ")
                val id = if (entries(0) endsWith "$")
                    entries(0).substring(0, entries(0).length - 1).toInt
                else
                    entries(0).toInt
                maxId = scala.math.max(id, maxId)
                val varname = if (entries(1).endsWith("_m")) entries(1).substring(0, entries(1).length - 2) + "_MODULE" else entries(1)
                if (variables contains varname)
                    assert(false, "variable " + varname + " declared twice")
                variables = variables.updated(varname, id)
            } else if ((line startsWith "p ") || (line.trim.size == 0)) {
                //comment, do nothing
            } else {
                val vec = new VecInt()
                for (literal <- line.split(" "))
                    if (literal != "0")
                        vec.push(literal.toInt)
                clauses.push(vec)
            }


        }
        assert(maxId == variables.size, "largest variable id " + maxId + " differs from number of variables " + variables.size)
        new SATFeatureModel(variables, clauses, maxId)
    }


    @Test
    @Ignore
    def testAgainstOld {
        val dimacs = "src/test/resources/2.6.33.3-2var.dimacs"
        val fm = createFromDimacsFile_2Var(dimacs)
        //        val workingDir = "Linux"
        val arch = "x86"
        val rsfFile = new File(workingDir, arch + ".rsf")

        val model = getModel(workingDir, kconfigFile(arch), rsfFile)


        for (i <- model.items.values.toList.sortBy(_.name)) {
            for (c <- i.getConstraints)
                if (!c.collectDistinctFeatures.exists(f => f.startsWith("CHOICE") || f.endsWith("_MODULE") || f == "MODULES"))
                    if (!c.isTautology())
                        if (!c.isTautology(fm)) {
                            print("#" + i.name)
                            println(" missing constraint: " + c)
                        }
            //                        else println("found: " + c)
        }
    }



}
