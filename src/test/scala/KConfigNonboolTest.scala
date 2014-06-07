package de.fosd.typechef.kconfig

import org.junit._

/**
 * brute force tests with many small handwritten kconfig files
 *
 */
class KConfigNonboolTest extends DifferentialTesting {

    @Test def testNonbool() {
        checkTestModelBruteForce("src/test/resources/nonbool.config")
    }

    @Test def testNumDep() {
        checkTestModelBruteForce("src/test/resources/numdep.config")
    }

    @Test def testStringDep() {
        checkTestModelBruteForce("src/test/resources/stringdep.config")
    }

    @Test def testNumDep2() {
        checkTestModelBruteForce("src/test/resources/numdep2.config")
    }

}
