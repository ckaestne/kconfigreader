package de.fosd.typechef.kconfig

import org.junit._
import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * brute force tests with many small handwritten kconfig files
 *
 */
class KConfigFragmentTest extends DifferentialTesting {

    @Test def testMiniKConfig() {
        checkTestModelBruteForce("src/test/resources/mini.config")
    }

    @Test def testMini2KConfig() {
        checkTestModelBruteForce("src/test/resources/mini2.config")
    }

    @Test def testMini3KConfig() {
        checkTestModelBruteForce("src/test/resources/mini3.config")
    }

    @Test def testDepKConfig() {
        checkTestModelBruteForce("src/test/resources/dep.config")
    }

    @Test def testSelKConfig() {
        checkTestModelBruteForce("src/test/resources/sel.config")
    }

    @Test def testSelHiddenKConfig() {
        checkTestModelBruteForce("src/test/resources/selhidden.config")
    }


    @Test def testSelHidden2KConfig() {
        checkTestModelBruteForce("src/test/resources/selhidden2.config")
    }

    @Test def testSelHidden3KConfig() {
        checkTestModelBruteForce("src/test/resources/selhidden3.config")
    }

    @Test def testSelHidden4KConfig() {
        checkTestModelBruteForce("src/test/resources/selhidden4.config")
    }

    @Test def testSelHidden5KConfig() {
        checkTestModelBruteForce("src/test/resources/selhidden5.config")
    }

    @Test def testChoiceKConfig() {
        checkTestModelBruteForce("src/test/resources/choice.config")
    }

    @Test
    @Ignore def testRandomFiles() {
        for (i <- 0 until 100)
            checkTestModelBruteForce("src/test/resources/gen/randombool%02d.conf".format(i))
    }

    @Ignore
    @Test def testInt() {
        checkTestModelBruteForce("src/test/resources/int.config")
    }

    @Ignore
    @Test def testHex() {
        checkTestModelBruteForce("src/test/resources/hex.config")
    }

    @Ignore
    @Test def testString() {
        checkTestModelBruteForce("src/test/resources/string.config")
    }

    @Test def testTri() {
        checkTestModelBruteForce("src/test/resources/tri.config")
    }

    @Test def testTriDep() {
        checkTestModelBruteForce("src/test/resources/tridep.config")
    }

    @Test def testTri2Dep() {
        checkTestModelBruteForce("src/test/resources/tridep2.config")
    }

    @Test def testTri3Dep() {
        checkTestModelBruteForce("src/test/resources/tridep3.config")
    }

    @Test def testTriSel() {
        checkTestModelBruteForce("src/test/resources/trisel.config")
    }

    @Test def testTriSel2() {
        checkTestModelBruteForce("src/test/resources/trisel2.config")
    }

    @Test def testTriSel3() {
        checkTestModelBruteForce("src/test/resources/trisel3.config")
    }

    @Test def testTri4Dep() {
        checkTestModelBruteForce("src/test/resources/tridep4.config")
    }

    @Test def testTri5Dep() {
        checkTestModelBruteForce("src/test/resources/tridep5.config")
    }

    @Test def testTriSel4() {
        checkTestModelBruteForce("src/test/resources/trisel4.config")
    }

    @Test def testTriHidden() {
        checkTestModelBruteForce("src/test/resources/trihidden.config")
    }

    @Test def testTriHidden2() {
        checkTestModelBruteForce("src/test/resources/trihidden2.config")
    }

    @Test def testTriHidden3() {
        checkTestModelBruteForce("src/test/resources/trihidden3.config")
    }

    @Test def testTriHidden4() {
        checkTestModelBruteForce("src/test/resources/trihidden4.config")
    }


    @Test def testTriSelHiddenKConfig() {
        checkTestModelBruteForce("src/test/resources/triselhidden.config")
    }


    @Test def testTriSelHidden2KConfig() {
        checkTestModelBruteForce("src/test/resources/triselhidden2.config")
    }

    @Test def testTriSelHidden3KConfig() {
        checkTestModelBruteForce("src/test/resources/triselhidden3.config")
    }

    @Test def testTriSelHidden4KConfig() {
        checkTestModelBruteForce("src/test/resources/triselhidden4.config")
    }

    @Test def testTriSelHidden5KConfig() {
        checkTestModelBruteForce("src/test/resources/triselhidden5.config")
    }

    @Test def testMenuconfig() {
        checkTestModelBruteForce("src/test/resources/menuconfig.config")
    }

    @Test def testChoice2() {
        checkTestModelBruteForce("src/test/resources/choice2.config")
    }

    @Test def testChoiceTri() {
        checkTestModelBruteForce("src/test/resources/choicetri.config")
    }

    @Test def testChoiceOpt() {
        checkTestModelBruteForce("src/test/resources/choiceopt.config")
    }


    @Test def testTriDepExtra() {
        checkTestModelBruteForce("src/test/resources/tridepextra.config")
    }

    @Test def testTriDepExtra2() {
        checkTestModelBruteForce("src/test/resources/tridepextra2.config")
    }

    @Test def testTriDepExtra3() {
        checkTestModelBruteForce("src/test/resources/tridepextra3.config")
    }

    @Test def testDepExtra() {
        checkTestModelBruteForce("src/test/resources/depextra.config")
    }

    @Test def testDepExtra2() {
        checkTestModelBruteForce("src/test/resources/depextra2.config")
    }

    @Test def testDepExtra3() {
        checkTestModelBruteForce("src/test/resources/depextra3.config")
    }
    @Ignore @Test def testNumDep() {
        checkTestModelBruteForce("src/test/resources/numdep.config")
    }
    @Ignore @Test def testStringDep() {
        checkTestModelBruteForce("src/test/resources/stringdep.config")
    }

    @Test def testChoiceDep() {
        checkTestModelBruteForce("src/test/resources/choicedep.config")
    }
    @Test def testDepHidden() {
        checkTestModelBruteForce("src/test/resources/dephidden.config")
    }
    @Test def testChoiceTriDep() {
        checkTestModelBruteForce("src/test/resources/choicetridep.config")
    }

}
