package de.fosd.typechef.kconfig

import org.junit._
import java.io._
import de.fosd.typechef.featureexpr.FeatureExprFactory
import scala._
import FeatureExprFactory._

class LinuxTest extends DifferentialTesting {


    @Test
    def testLoadLinux() {
        val workingDir = new File(linuxTreeRoot)
        //            val kconfigFile = "arch/x86/Kconfig"
        val rsfFile = new File(workingDir, "x86.rsf")
        val model = new RSFReader().readRSF(rsfFile)

        //            println(model.getConstraints.forall(_.isSatisfiable()))

        //            var f=model.getConstraints.take(6000).reduce(_ and _)
        //            for (c<-model.getConstraints.drop(6000)) {
        //                println(c)
        //                f=f and c
        //                if (!f.isSatisfiable())
        //                    throw new RuntimeException("unsat with "+c)
        //            }

        var f = model.items.values.flatMap(_.getConstraints).reduce(_ and _)
        assert(f.isSatisfiable())
        for (c <- model.choices.values) {
            println("#"+c.name)
            for (x <- c.getConstraints) {
                println(x)
                f = f and x
                if (!f.isSatisfiable())
                    throw new RuntimeException("unsat with " + c)
            }
        }

        //            writeModel(workingDir, model)

        //            genAllCombinationsFromPartial(kconfigFile, workingDir, model, Set("FEATURE_CHECK_UNICODE_IN_ENV", "UNICODE_SUPPORT", "UNICODE_USING_LOCALE"))
    }


    def writeModel(workingDir: File, model: KConfigModel) {
        val writer = new FileWriter(new File(workingDir, "x86.model"))
        for (i <- model.items.values) {
            writer.write("#item " + i.name + "\n")
            i.getConstraints.map(s => if (!s.isTautology()) writer.write(s + "\n"))
        }
        for (i <- model.choices.values) {
            writer.write("#choice " + i.name + "\n")
            i.getConstraints.map(s => if (!s.isTautology()) writer.write(s + "\n"))
        }
        writer.close()
    }
}
