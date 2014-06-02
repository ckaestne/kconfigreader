package de.fosd.typechef.kconfig

import org.junit._
import java.io._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import FeatureExprFactory._
import scala._
import scala.sys.process.Process

class BusyboxTest extends DifferentialTesting {


        @Test
        def testBusybox() {
            val workingDir = new File("/usr0/home/ckaestne/work/TypeChef/BusyboxAnalysis/gitbusybox/")
            val kconfigFile = "Config.in"
            val model = getModel(workingDir, kconfigFile)
            genAllCombinationsFromPartial(kconfigFile, workingDir, model, Set("FEATURE_CHECK_UNICODE_IN_ENV", "UNICODE_SUPPORT", "UNICODE_USING_LOCALE"))
        }

    @Test
    def testAgainstPrior {
        val oldFM = "src/test/resources/featureModel.dimacs"
       val fm= FeatureExprFactory.dflt.featureModelFactory.createFromDimacsFile(oldFM,"")

        val workingDir = new File("/home/energy/BusyboxAnalysis/gitbusybox/")
        val kconfigFile = "Config.in"
        val model = getModel(workingDir, kconfigFile)


//        for (c<-model.getConstraints)
//            if (!c.collectDistinctFeatures.exists(_.startsWith("CHOICE")))
//                if (!c.isTautology(fm)) {
//                    println("missing constraint: "+c)
//                }

    }

}
