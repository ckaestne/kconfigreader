package de.fosd.typechef.kconfig

import org.junit.Test


class UndertakerTests2 extends DifferentialTesting {
    @Test def test14_multia() {
        checkTestModelBruteForce("src/test/resources/undertaker2/multi-invisible/multia.fm")
    }
    @Test def test15_multiinvisible() {
        checkTestModelBruteForce("src/test/resources/undertaker2/multi-invisible/multiinvisible.fm")
    }
    @Test def test16_selectif() {
        checkTestModelBruteForce("src/test/resources/undertaker2/selectif-bool/selectif.fm")
    }
    @Test def test17_simple_depends_tristate() {
        checkTestModelBruteForce("src/test/resources/undertaker2/depends-tristate/simple_depends_tristate.fm")
    }
    @Test def test18_linux_part() {
        checkTestModelBruteForce("src/test/resources/undertaker2/select-linux/linux-part.fm")
    }
    @Test def test19_defect() {
        checkTestModelBruteForce("src/test/resources/undertaker2/defect/defect.fm")
    }
    @Test def test20_expressions() {
        checkTestModelBruteForce("src/test/resources/undertaker2/multidef-depend1-bool/expressions.fm")
    }
    @Test def test21_not() {
        checkTestModelBruteForce("src/test/resources/undertaker2/not-tristate/not.fm")
    }
    @Test def test22_mtd() {
        checkTestModelBruteForce("src/test/resources/undertaker2/choice-depends-mix/mtd.fm")
    }
    @Test def test24_choices() {
        checkTestModelBruteForce("src/test/resources/undertaker2/choice-bool/choices.fm")
    }
    @Test def test25_multidep() {
        checkTestModelBruteForce("src/test/resources/undertaker2/multidepends-bool/multidep.fm")
    }
    @Test def test26_expressions() {
        checkTestModelBruteForce("src/test/resources/undertaker2/multidef-depend0-bool/expressions.fm")
    }
    @Test def test27_expressions() {
        checkTestModelBruteForce("src/test/resources/undertaker2/multidef-depend0-tristate/expressions.fm")
    }
    @Test def test28_promptif_bool() {
        checkTestModelBruteForce("src/test/resources/undertaker2/prompt-if-bool/promptif-bool.fm")
    }
    @Test def test29_expressions() {
        checkTestModelBruteForce("src/test/resources/undertaker2/expression-bool/expressions.fm")
    }
    @Test def test30_promptif_bool() {
        checkTestModelBruteForce("src/test/resources/undertaker2/prompt-if-bool-mix/promptif-bool.fm")
    }
    @Test def test31_promptif_tristate() {
        checkTestModelBruteForce("src/test/resources/undertaker2/prompt-if-tristate/promptif-tristate.fm")
    }
    @Test def test32_allways_on() {
        checkTestModelBruteForce("src/test/resources/undertaker2/allways-on/allways-on.fm")
    }
    @Test def test33_simple_depends_bool() {
        checkTestModelBruteForce("src/test/resources/undertaker2/depends-bool/simple_depends_bool.fm")
    }
}
