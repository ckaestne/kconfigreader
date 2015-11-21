package de.fosd.typechef.kconfig

import org.junit._

/**
  * brute force tests with
  * the test cases from Sascha El-Sharkawy's paper
  * El-Sharkawy, Sascha, Adam Krafczyk, and Klaus Schmid. "Analysing the Kconfig semantics and its analysis tools." Proceedings of the 2015 ACM SIGPLAN International Conference on Generative Programming: Concepts and Experiences. ACM, 2015.
  * https://dl.acm.org/citation.cfm?id=2814222
  *
  * See also the technical report
  * https://sse.uni-hildesheim.de/media/fb4/informatik/AG_SSE/PDFs/publications/Kconfig/Report.pdf
  */
class SaschasTests extends DifferentialTesting {
    @Test def test1_Kconfig_3_3_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.3.txt")
    }

    @Ignore("invalid input file due to cyclic dependencies")
    @Test def test2_Kconfig_3_11_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.11.txt")
    }

    @Test def test3_Kconfig_3_15_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.15.txt")
    }

    @Test def test4_Kconfig_3_7_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.7.txt")
    }

    @Test def test5_Kconfig_3_4_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.4.txt")
    }

    @Test def test6_Kconfig_3_8_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.8.txt")
    }

    @Ignore("does not seem expressible in propositional logic; " +
        "elements of a choice need to be selected if any of them is selectable")
    @Test def test7_Kconfig_3_12_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.12.txt")
    }

    @Ignore("does not seem expressible in propositional logic; " +
        "elements of a choice need to be selected if any of them is selectable")
    @Test def test7_Kconfig_3_12a_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.12a.txt")
    }

    @Test def test8_Kconfig_3_13_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.13.txt")
    }

    @Ignore("invalid input file due to cyclic dependencies")
    @Test def test9_Kconfig_3_14_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.14.txt")
    }

    @Test def test10_Kconfig_3_15_2_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.15-2.txt")
    }

    @Test def test11_Kconfig_3_5_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.5.txt")
    }

    @Ignore("renamed MODULES option explicitly not supported, see readme")
    @Test def test12_Kconfig_3_2_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.2.txt")
    }

    @Test def test13_Kconfig_3_6_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.6.txt")
    }

    @Test def test14_Kconfig_3_10_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.10.txt")
    }

    @Test def test15_Kconfig_3_9_() {
        checkTestModelBruteForce("src/test/resources/sascha/Kconfig_3.9.txt")
    }
}


//object GenUndertakerTests extends App {
//
//    val files1: List[File] = new File("src/test/resources/sascha/").listFiles().filter(_.getName.endsWith(".txt")).toList
//    var i=0;
//    for (v <- (files1)) {
//        i+=1
//        val name = v.getName.dropRight(3).replace("-", "_").replace(".","_")
//        println("@Test def test%d_%s() { checkTestModelBruteForce(\"%s\") }".format(i,name, v.getPath))
//    }
//
//}


