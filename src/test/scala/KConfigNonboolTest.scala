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

    @Ignore @Test def testNumDep() {
        checkTestModelBruteForce("src/test/resources/numdep.config")
    }
    @Ignore @Test def testStringDep() {
        checkTestModelBruteForce("src/test/resources/stringdep.config")
    }

}
