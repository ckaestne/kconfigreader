package de.fosd.typechef.kconfig

import org.junit._
import java.io._
import de.fosd.typechef.featureexpr.FeatureExprFactory
import scala._
import FeatureExprFactory._
import de.fosd.typechef.featureexpr.sat.SATFeatureExpr

class LinuxTest extends DifferentialTesting {

    lazy val x86model = getModel("x86")
    lazy val x86kconfig = kconfigFile("x86")
    def kconfigFile(arch:String) = "arch/"+arch+"/Kconfig"

    val workingDir = new File(linuxTreeRoot)
    def getModel(arch: String):KConfigModel ={

        val rsfFile = new File(workingDir, arch+".rsf")

        getModel(workingDir,kconfigFile(arch), rsfFile)
    }

    @Test
    def testLoadLinux() {
        for (arch<-List("x86","arm")) {
            //        FeatureExprFactory.setDefault(FeatureExprFactory.bdd)
            val model = getModel(arch)
            val allconstraints = model.getConstraints

            assert(allconstraints.forall(_.isSatisfiable()), "extracted constraint is not satisfiable")
            assert(allconstraints.reduce(_ and _).isSatisfiable(), "extracted model is not satisfiable")


            writeModel(arch, workingDir, model)

            //        println(".")
            //        var count = 0
            //        allconstraints.map(c=>{if (c.collectDistinctFeatures.size>16) println(c.collectDistinctFeatures.size) else c.asInstanceOf[SATFeatureExpr].toCNF()})

            //            genAllCombinationsFromPartial(kconfigFile, workingDir, model, Set("FEATURE_CHECK_UNICODE_IN_ENV", "UNICODE_SUPPORT", "UNICODE_USING_LOCALE"))
        }
    }

    @Test
    def test32vs64() {

        genAllCombinationsFromPartial(x86kconfig, workingDir, x86model,
            Set(/*"X86_32","X86_64"*/))
    }


    def writeModel(arch: String, workingDir: File, model: KConfigModel) {
        val writer = new FileWriter(new File(workingDir, arch+".model"))
        for (i <- model.items.values.toList.sortBy(_.name)) {
            writer.write("#item " + i.name + "\n")
            i.getConstraints.map(s => if (!s.isTautology()) writer.write(s + "\n"))
        }
        for (i <- model.choices.values.toList.sortBy(_.name)) {
            writer.write("#choice " + i.name + "\n")
            i.getConstraints.map(s => if (!s.isTautology()) writer.write(s + "\n"))
        }
        writer.close()
    }
}
