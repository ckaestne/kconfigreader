package de.fosd.typechef.kconfig

import org.junit._
import java.io._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import FeatureExprFactory._
import scala._
import scala.sys.process.Process

/**
 * differential testing infrastructure and tests for kconfig
 *
 * analyzes all configurations of all features (or a subset of features)
 * in a kconfig file
 *
 * it computes the answer based on the model derived from the kconfig
 * file and checks that answer against running kconfig (the conf tool
 * with --olddefconfig option) which would fix invalid inputs.
 *
 * the kconfig extraction is broken if there is at least one configuration
 * for which kconfig returns a different value than the derived
 * model
 *
 */
class BusyboxTest extends DifferentialTesting {


        @Test
        def testBusybox() {
            val workingDir = new File("/usr0/home/ckaestne/work/TypeChef/BusyboxAnalysis/gitbusybox/")
            val kconfigFile = "Config.in"
            val model = getModel(workingDir, kconfigFile)
            genAllCombinationsFromPartial(kconfigFile, workingDir, model, Set("FEATURE_CHECK_UNICODE_IN_ENV", "UNICODE_SUPPORT", "UNICODE_USING_LOCALE"))
        }


}
