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
    @Test def testNumDep2Named() {
        checkTestModelBruteForce("src/test/resources/numdep2named.config")
    }

    @Test def testRange() {
        checkTestModelBruteForce("src/test/resources/range.config")
    }
    @Test def testRange2() {
        checkTestModelBruteForce("src/test/resources/range2.config")
    }
    @Test def testRange3() {
        checkTestModelBruteForce("src/test/resources/range3.config")
    }
}
