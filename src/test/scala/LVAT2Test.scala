import de.fosd.typechef.kconfig.DifferentialTesting
import org.junit._

/**
 * run the secret LVAT test cases
 */
class LVAT2Test extends DifferentialTesting {


    @Test def testLVATChoice() {
        checkTestModelBruteForce("src/test/resources/lvat2/choice.Kconfig")
    }

    @Test def testLVATChoiceConstr() {
        checkTestModelBruteForce("src/test/resources/lvat2/choice-constraint.Kconfig")
    }

    @Test def testLVATChoiceDiff() {
        checkTestModelBruteForce("src/test/resources/lvat2/choice-different.Kconfig")
    }

    @Test@Ignore("circular dependencies ignored, reported as error by kconfig; TODO: detect as smell") def testLVAT3() {
            checkTestModelBruteForce("src/test/resources/lvat2/circular.Kconfig")
    }

    @Test def testLVAT4() {
        checkTestModelBruteForce("src/test/resources/lvat2/dependency.Kconfig")
    }

    //TODO: tristate visibility is an issue. allow only M if dependent on element with M
    @Test def testLVAT4a() {
        checkTestModelBruteForce("src/test/resources/lvat2/dependency2.Kconfig")
    }

    @Test def testLVAT5() {
        checkTestModelBruteForce("src/test/resources/lvat2/dupe.Kconfig")
    }

    @Test def testLVAT6() {
        checkTestModelBruteForce("src/test/resources/lvat2/groups.Kconfig")
    }

    @Test def testLVAT7() {
        checkTestModelBruteForce("src/test/resources/lvat2/hex-bad.Kconfig")
    }

    @Test def testLVAT8() {
        checkTestModelBruteForce("src/test/resources/lvat2/hex.Kconfig")
    }

    @Test def testLVAT8a() {
        checkTestModelBruteForce("src/test/resources/lvat2/hex2.Kconfig")
    }

    @Test def testLVAT9() {
        checkTestModelBruteForce("src/test/resources/lvat2/linux-3.0.Kconfig")
    }

    @Test def testLVAT10() {
        checkTestModelBruteForce("src/test/resources/lvat2/multiple-2.Kconfig")
    }

    @Test def testLVAT11() {
        checkTestModelBruteForce("src/test/resources/lvat2/multiple-3.Kconfig")
    }

    @Test def testLVAT12() {
        checkTestModelBruteForce("src/test/resources/lvat2/multiple.Kconfig")
    }

    @Test def testLVAT13() {
        checkTestModelBruteForce("src/test/resources/lvat2/nesting.Kconfig")
    }

    @Test def testLVAT14() {
        checkTestModelBruteForce("src/test/resources/lvat2/protoconf.Kconfig")
    }

    @Test def testLVAT15() {
        checkTestModelBruteForce("src/test/resources/lvat2/string.Kconfig")
    }

    @Test@Ignore("too big to run brute force") def testLVAT16() {
        checkTestModelBruteForce("src/test/resources/lvat2/thesis.Kconfig")
    }

    @Test def testLVAT17() {
        checkTestModelBruteForce("src/test/resources/lvat2/type-error.Kconfig")
    }

    //    choice-constraint.Kconfig  groups.Kconfig      multiple.Kconfig
    //    choice-different.Kconfig   hex-bad.Kconfig     nesting.Kconfig
    //    choice.Kconfig             hex.Kconfig         protoconf.Kconfig
    //    circular.Kconfig           linux-3.0.Kconfig   string.Kconfig
    //    dependency.Kconfig         multiple-2.Kconfig  thesis.Kconfig
    //    dupe.Kconfig               multiple-3.Kconfig  type-error.Kconfig


}
