//package de.fosd.typechef.kconfig
//
//import org.junit.Test
//import org.junit.runner.RunWith
//import org.junit.runners.Parameterized
//import org.junit.runners.Parameterized.Parameters
//import java.io.File
//
///**
// * runs all .config fragment tests nested inside another if
// * to check whether it still works with those dependencies
// *
//        currently deactivated, because kconfig behaves weiredly
//        when MODULES is nested in a tristate option
// */
//
//@RunWith(value = classOf[Parameterized])
//class NestTests(fmFile: File) extends DifferentialTesting {
//    @Test def runTest = {
//        println(fmFile)
//        checkTestModelBruteForce(fmFile.getPath)
//    }
//
//
//}
//
//object NestTests {
//
//
//    @Parameters def parameters: java.util.Collection[Array[File]] = {
//        val r = new java.util.ArrayList[Array[File]]()
//        for (v <- new File("src/test/resources/nest/").listFiles().filter(_.getName.endsWith(".config")))
//            r.add(Array(v))
//        r
//    }
//
//}
//
