package de.fosd.typechef.kconfig

import org.junit.Test

/**
 * brute force tests with
 * the test cases from the undertaker repo
 *
 *
 */
class UndertakerTests extends DifferentialTesting {
    @Test def test1_choice() {
        checkTestModelBruteForce("src/test/resources/undertaker/choice.fm")
    }
    @Test def test2_choice2() {
        checkTestModelBruteForce("src/test/resources/undertaker/choice2.fm")
    }
    @Test def test3_multiple_prompts() {
        checkTestModelBruteForce("src/test/resources/undertaker/multiple-prompts.fm")
    }
    @Test def test4_default_doubled() {
        checkTestModelBruteForce("src/test/resources/undertaker/default-doubled.fm")
    }
    @Test def test5_equals() {
        checkTestModelBruteForce("src/test/resources/undertaker/equals.fm")
    }
    @Test def test6_multiple_depends() {
        checkTestModelBruteForce("src/test/resources/undertaker/multiple-depends.fm")
    }
    @Test def test7_depends_on_m() {
        checkTestModelBruteForce("src/test/resources/undertaker/depends-on-m.fm")
    }
    @Test def test8_main() {
        checkTestModelBruteForce("src/test/resources/undertaker/main.fm")
    }
    @Test def test9_test2() {
        checkTestModelBruteForce("src/test/resources/undertaker/test2.fm")
    }
    @Test def test10_sync() {
        checkTestModelBruteForce("src/test/resources/undertaker/sync.fm")
    }
    @Test def test11_test1() {
        checkTestModelBruteForce("src/test/resources/undertaker/test1.fm")
    }
    @Test def test12_test3() {
        checkTestModelBruteForce("src/test/resources/undertaker/test3.fm")
    }
    @Test def test13_select() {
        checkTestModelBruteForce("src/test/resources/undertaker/select.fm")
    }
}


//object GenUndertakerTests extends App {
//
//    val files1: List[File] = new File("src/test/resources/undertaker/").listFiles().filter(_.getName.endsWith(".fm")).toList
//
//
//    val files2 =
//        for (dir <- new File("src/test/resources/undertaker2/").listFiles() if dir.isDirectory;
//             v <- dir.listFiles if v.getName.endsWith(".fm"))
//        yield v
//
//    var i=0;
//    for (v <- (files1 ++ files2)) {
//        i+=1
//        val name = v.getName.dropRight(3).replace("-", "_")
//        println("@Test def test%d_%s() { checkTestModelBruteForce(\"%s\") }".format(i,name, v.getPath))
//    }
//
//
//}
//
//
