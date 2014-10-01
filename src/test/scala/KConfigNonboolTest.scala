package de.fosd.typechef.kconfig

import org.junit._

/**
 * brute force tests with many small handwritten kconfig files using nonboolean options
 * (strings, integer, hex, and constraints among them)
 *
 */
class KConfigNonboolTest extends DifferentialTesting {

    @Test def testNonbool() {
        checkTestModelBruteForce("src/test/resources/nonbool.config")
    }

    @Test def testNumDep() {
        checkTestModelBruteForce("src/test/resources/numdep.config")
    }

    @Test def testIntDep2() {
        checkTestModelBruteForce("src/test/resources/intdep2.config")
    }


    @Test def testIntDep3() {
        checkTestModelBruteForce("src/test/resources/intdep3.config")
    }

    @Test def testStringDep() {
        checkTestModelBruteForce("src/test/resources/stringdep.config")
    }

    @Test def testStringDep2() {
        checkTestModelBruteForce("src/test/resources/stringdep2.config")
    }

    @Test def testStringDep3() {
        checkTestModelBruteForce("src/test/resources/stringdep3.config")
    }
    @Test def testStringDep4() {
        checkTestModelBruteForce("src/test/resources/stringdep4.config")
    }
    @Test def testStringDep5() {
        checkTestModelBruteForce("src/test/resources/stringdep5.config")
    }
    @Test def testStringDep6() {
        checkTestModelBruteForce("src/test/resources/stringdep6.config")
    }
    @Test def testStringDep7() {
        checkTestModelBruteForce("src/test/resources/stringdep7.config")
    }
    @Test def testStringDep8() {
        checkTestModelBruteForce("src/test/resources/stringdep8.config")
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
    @Test def testRange4() {
        checkTestModelBruteForce("src/test/resources/range4.config")
    }
    @Test def testDynamicRange() {
        checkTestModelBruteForce("src/test/resources/dynamicrange.config")
    }
    @Test def testHexRange() {
        checkTestModelBruteForce("src/test/resources/hexrange.config")
    }
}
