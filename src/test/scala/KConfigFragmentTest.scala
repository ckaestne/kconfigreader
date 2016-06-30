package de.fosd.typechef.kconfig

import org.junit._

/**
  * brute force tests with many small handwritten kconfig files
  *
  */
class KConfigFragmentTest extends DifferentialTesting {

    @Test def testMini() {
        checkTestModelBruteForce("src/test/resources/mini.config")
    }

    @Test def testMini2() {
        checkTestModelBruteForce("src/test/resources/mini2.config")
    }

    @Test def testMini3() {
        checkTestModelBruteForce("src/test/resources/mini3.config")
    }

    @Test def testDep() {
        checkTestModelBruteForce("src/test/resources/dep.config")
    }

    @Test def testDep2() {
        checkTestModelBruteForce("src/test/resources/dep2.config")
    }

    @Test def testSel() {
        checkTestModelBruteForce("src/test/resources/sel.config")
    }

    @Test def testSelHidden() {
        checkTestModelBruteForce("src/test/resources/selhidden.config")
    }


    @Test def testSelHidden2() {
        checkTestModelBruteForce("src/test/resources/selhidden2.config")
    }

    @Test def testSelHidden3() {
        checkTestModelBruteForce("src/test/resources/selhidden3.config")
    }

    @Test def testSelHidden4() {
        checkTestModelBruteForce("src/test/resources/selhidden4.config")
    }

    @Test def testSelHidden5() {
        checkTestModelBruteForce("src/test/resources/selhidden5.config")
    }

    @Test def testChoice() {
        checkTestModelBruteForce("src/test/resources/choice.config")
    }

    @Test
    @Ignore def testRandomFiles() {
        for (i <- 0 until 100)
            checkTestModelBruteForce("src/test/resources/gen/randombool%02d.conf".format(i))
    }

    @Test def testInt() {
        checkTestModelBruteForce("src/test/resources/int.config")
    }

    @Test
    @Ignore("issue in `conf --olddefconfig` that doesn't mark invalid configurations correctly") def testInt2() {
        checkTestModelBruteForce("src/test/resources/int_fail.config")
    }

    @Test def testHex() {
        checkTestModelBruteForce("src/test/resources/hex.config")
    }

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


    @Test def testTriSelHiddenK() {
        checkTestModelBruteForce("src/test/resources/triselhidden.config")
    }


    @Test def testTriSelHidden2K() {
        checkTestModelBruteForce("src/test/resources/triselhidden2.config")
    }

    @Test def testTriSelHidden3K() {
        checkTestModelBruteForce("src/test/resources/triselhidden3.config")
    }

    @Test def testTriSelHidden4K() {
        checkTestModelBruteForce("src/test/resources/triselhidden4.config")
    }

    @Test def testTriSelHidden5K() {
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


    @Test def testChoiceDep() {
        checkTestModelBruteForce("src/test/resources/choicedep.config")
    }

    @Test def testChoiceDep2() {
        checkTestModelBruteForce("src/test/resources/choicedep2.config")
    }

    @Test def testDepHidden() {
        checkTestModelBruteForce("src/test/resources/dephidden.config")
    }

    @Test def testChoiceTriDep() {
        checkTestModelBruteForce("src/test/resources/choicetridep.config")
    }

    @Test def testPromptIf() {
        checkTestModelBruteForce("src/test/resources/promptif.config")
    }

    @Test def testUndef() {
        checkTestModelBruteForce("src/test/resources/undef.config")
    }

    @Test def testTriBool() {
        checkTestModelBruteForce("src/test/resources/tribool.config")
    }

    @Test def testUndefSelect() {
        checkTestModelBruteForce("src/test/resources/undefselect.config")
    }

    @Ignore("this is imperative kconfig behavior not occuring in practice")
    @Test def testTriBool2() {
        checkTestModelBruteForce("src/test/resources/tribool2.config")
    }

    @Test def testChoiceNamed() {
        checkTestModelBruteForce("src/test/resources/choicenamed.config")
    }

    @Test def testNoChoice() {
        checkTestModelBruteForce("src/test/resources/nochoice.config")
    }

    @Test def testChoiceTriDefault() {
        checkTestModelBruteForce("src/test/resources/choicetridefault.config")
    }

    @Test def testChoiceDefault() {
        checkTestModelBruteForce("src/test/resources/choicedefault.config")
    }

    @Test def testSelectOverDepends() {
        checkTestModelBruteForce("src/test/resources/selectoverdepends.config")
    }

    @Test def testSelectOverDepends2() {
        checkTestModelBruteForce("src/test/resources/selectoverdepends2.config")
    }

    @Test def testTriHiddenSelect() {
        checkTestModelBruteForce("src/test/resources/trihiddenselect.config")
    }

    @Test def testPageGuard() {
        checkTestModelBruteForce("src/test/resources/pageguard.config")
    }

    @Test def testTriMenuVisible() {
        checkTestModelBruteForce("src/test/resources/trimenuvisible.config")
    }

    @Ignore("not fully supported yet, needs fixing")
    @Test def testInvalidRange() {
        checkTestModelBruteForce("src/test/resources/invalidrange.config")
    }

    @Test def testHugeRange() {
        checkTestModelBruteForce("src/test/resources/hugerange.config")
    }

    @Test def testContradiction() {
        checkTestModelBruteForce("src/test/resources/contradiction.config")
    }

    //    TODO check IO_DELAY_0X80 and DEFAULT_IO_DELAY_TYPE relationship in arch/x86/Kconfig.debug

}
