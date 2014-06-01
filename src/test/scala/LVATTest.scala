package de.fosd.typechef.kconfig

import org.junit.Test

/**
 * run the single lvat test case
 */
class LVATTest extends DifferentialTesting {


    @Test def testLVAT() {
        checkTestModelBruteForce("src/test/resources/lvat/multipleDefinitions-1.Kconfig")
    }

    @Test def testLVATtmp() {
        checkTestModelBruteForce("src/test/resources/undertaker/test2.fm")
    }
    @Test def testChoice() {
        checkTestModelBruteForce("src/test/resources/undertaker/choice.fm")
    }
    @Test def testDefaultDoubled() {
        checkTestModelBruteForce("src/test/resources/undertaker/default-doubled.fm")
    }

}
