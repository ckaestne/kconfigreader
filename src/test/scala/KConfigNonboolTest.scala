package de.fosd.typechef.kconfig
import org.junit._

/**
 * brute force tests with many small handwritten kconfig files
 *
 */
class KConfigNonboolTest extends DifferentialTesting {

    @Test def testMiniKConfig() {
        checkTestModelBruteForce("src/test/resources/nonbool.config")
    }

}
