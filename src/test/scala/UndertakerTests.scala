package de.fosd.typechef.kconfig

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters
import java.io.File

/**
 * brute force tests with
 * the test cases from the undertaker repo
 *
 *
 */

@RunWith(value = classOf[Parameterized])
class UndertakerTests(fmFile: File) extends DifferentialTesting {
    @Test def runTest = {
        println(fmFile)
        checkTestModelBruteForce(fmFile.getPath)
    }


}

object UndertakerTests {


    @Parameters def parameters: java.util.Collection[Array[File]] = {
        val r = new java.util.ArrayList[Array[File]]()
        for (v <- new File("src/test/resources/undertaker/").listFiles().filter(_.getName.endsWith(".fm")))
            r.add(Array(v))
        r
    }

}


@RunWith(value = classOf[Parameterized])
class UndertakerTests2(fmFile: File) extends DifferentialTesting {
    @Test def runTest = {
        println(fmFile)
        checkTestModelBruteForce(fmFile.getPath)
    }


}

object UndertakerTests2 {


    @Parameters def parameters: java.util.Collection[Array[File]] = {
        val r = new java.util.ArrayList[Array[File]]()
        for (dir <- new File("src/test/resources/undertaker2/").listFiles() if dir.isDirectory;
             v <- dir.listFiles if v.getName.endsWith(".fm"))
            r.add(Array(v))
        r
    }

}
