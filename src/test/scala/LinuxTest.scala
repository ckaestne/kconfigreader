package de.fosd.typechef.kconfig

import org.junit._
import java.io._
import de.fosd.typechef.featureexpr.{FeatureModel, FeatureExpr, FeatureExprFactory, FeatureExprParser}
import scala._
import org.sat4j.core.{VecInt, Vec}
import org.sat4j.specs.IVecInt
import de.fosd.typechef.featureexpr.sat.{SATFeatureModel, SATFeatureExpr}
import scala.io.Source

trait LinuxTestInfrastructure extends DifferentialTesting {

    def kconfigFile(arch: String) = "arch/" + arch + "/Kconfig"
    val workingDir = new File(linuxTreeRoot)

    override def dumpconfTool = (sys.env.getOrElse("DUMPCONF26333", "$PWD/binary/dumpconf-26333") + " %s").replace("$PWD", new File(".").getAbsolutePath)

    lazy val x86model = getModel("x86")
    lazy val x86kconfig = kconfigFile("x86")
    lazy val x86fm: FeatureExpr = x86model.getFM
    lazy val x86fm_dimacs: FeatureModel = {
        val dimacsfile = new File(workingDir, "x86.dimacs")
        new DimacsWriter().writeAsDimacs2(x86model.getConstraints.map(_.asInstanceOf[SATFeatureExpr]), dimacsfile)
        FeatureExprFactory.dflt.featureModelFactory.createFromDimacsFilePrefix(dimacsfile.getAbsolutePath, "")
    }
    lazy val armmodel = getModel("arm")
    lazy val armkconfig = kconfigFile("arm")


    def getModel(arch: String): KConfigModel = {

        val rsfFile = new File(workingDir, arch + ".rsf")

        getModel(workingDir, kconfigFile(arch), rsfFile)
    }
}

/**
 * some test cases that run against the linux feature model
 *
 * since this requires a larger setup and quite some time, these
 * tests are deactived by default
 */
class LinuxTest extends LinuxTestInfrastructure {


    //    @BeforeClass
    //    def generateLinuxX86FeatureModel


    @Test
    def testLoadLinux() {
        //just creating dimacs files for two architectures to see whether everything is working fine
        for (arch <- List("x86", "arm")) {
            println("getting model")
            val model = getModel(arch)

            println("getting constraints")
            val allconstraints = model.getConstraints

            println("checking combined constraint")
            val isSat = allconstraints.reduce(_ and _).isSatisfiable()
            if (!isSat) {
                println("checking each constraint")
                assert(allconstraints.forall(_.isSatisfiable()), "extracted constraint is not satisfiable")
            }
            assert(isSat, "extracted model is not satisfiable")
            //

            println("writing model")
            KConfigReader.writeModel(new File(workingDir, arch + ".model"), model)

            println("writing nonboolean")
            KConfigReader.writeNonBoolean(model, new File(workingDir, arch + ".nonbool.h"))

            //            println("reducing constraints for dimacs")
            //            val reducedconstraints = reduceConstraints(allconstraints)
            println("writing dimacs")
            new DimacsWriter().writeAsDimacs2(allconstraints.map(_.asInstanceOf[SATFeatureExpr]), new File(workingDir, arch + ".dimacs"))


        }
    }

    @Test
    def testMin() {
        //test a random valid configuration
        genAllCombinationsFromPartial(x86kconfig, workingDir, x86model,
            Set("USB_GADGET_LANGWELL"))
    }


    @Test
    def test32vs64() {
        genAllCombinationsFromPartial(x86kconfig, workingDir, x86model,
            Set("X86_32", "X86_64"))
    }


    @Test
    def test_regression() {
        //these previously triggered bugs, fixed now
        genAllCombinationsFromPartial(x86kconfig, workingDir, x86model,
            Set("PANEL"))
        genAllCombinationsFromPartial(x86kconfig, workingDir, x86model,
            Set("RADIO_RTRACK2"))
        genAllCombinationsFromPartial(x86kconfig, workingDir, x86model,
            Set("MPENTIUM4"))
        genAllCombinationsFromPartial(x86kconfig, workingDir, x86model,
            Set("INITRAMFS_COMPRESSION_GZIP"))
    }


    @Test
    def testArm() {
        genAllCombinationsFromPartial(armkconfig, workingDir, armmodel,
            Set())
    }


    @Test
    @Ignore("unnecessary, covered now by intdep2 and intdep3")
    def test_Panel_manual() {
        //this tests some weired issues with constraints that were not respected when generating random configurations
        def d = FeatureExprFactory.createDefinedExternal _

        val fm = x86fm_dimacs

        def expectSat(fexpr: FeatureExpr, isSat: Boolean = true) = {
            assert(fexpr.isSatisfiable(fm) == isSat, fexpr + (if (isSat) " not" else "") + " satisfiable")
            println(fexpr + (if (!isSat) " not" else "") + " satisfiable")
        }
        def expectContr(fexpr: FeatureExpr) = expectSat(fexpr, false)

        expectContr(d("PANEL_LCD_HEIGHT=0") and d("PANEL_PROFILE=0") and d("PANEL_LCD=1") and d("PANEL"))
        expectSat(d("PANEL_LCD_HEIGHT=40") and d("PANEL_PROFILE=0") and d("PANEL_LCD=1") and d("PANEL"))
        expectContr(d("PANEL_LCD_HEIGHT=0") and d("PANEL_PROFILE=0") and d("PANEL_LCD=1") and d("PANEL_MODULE"))



        expectContr(d("X86_INTERNODE_CACHE_SHIFT=0"))
        expectContr(d("X86_L1_CACHE_SHIFT=0"))
        expectContr(d("X86_L1_CACHE_SHIFT=4") and d("MPENTIUM4"))
        expectContr(d("X86_L1_CACHE_SHIFT=6") and d("MPENTIUM4"))
        expectSat(d("X86_L1_CACHE_SHIFT=7") and d("MPENTIUM4"))
        expectSat(d("X86_INTERNODE_CACHE_SHIFT=7") and d("X86_L1_CACHE_SHIFT=7"))
        expectContr(d("X86_INTERNODE_CACHE_SHIFT=4") and d("X86_L1_CACHE_SHIFT=5"))
        expectSat(d("X86_INTERNODE_CACHE_SHIFT=7") and d("X86_L1_CACHE_SHIFT=6"))
        expectContr(d("X86_INTERNODE_CACHE_SHIFT=7") and d("X86_L1_CACHE_SHIFT=6") and d("MPENTIUM4") andNot d("NUMA") andNot d("X86_VSMP"))
        expectSat(d("X86_INTERNODE_CACHE_SHIFT=7") and d("X86_L1_CACHE_SHIFT=6") and d("MK7") and d("NUMA"))
    }


    @Test
    def approxfmTest() {
        //approx.fm contained several handwritten constraints that are known to hold in the linux kernel

        for (lineOrig <- Source.fromFile("src/test/resources/approx.fm").getLines()) {
            val line = if (lineOrig contains "//") lineOrig.take(lineOrig indexOf "//") else lineOrig
            val lineExpr = new FeatureExprParser().parse(line)
            println(line)
            assert(lineExpr.isTautology(x86fm_dimacs), "approx.fm line '%s' was not guaranteed by feature model".format(lineExpr))
        }


    }


    @Test
    @Ignore
    def testAgainstOld {


        val dimacs = "src/test/resources/2.6.33.3-2var.dimacs"
        val fm = createFromDimacsFile_2Var(dimacs)
        //        val workingDir = "Linux"
        val arch = "x86"
        val rsfFile = new File(workingDir, arch + ".rsf")

        val model = getModel(workingDir, kconfigFile(arch), rsfFile)


        for (i <- model.items.values.toList.sortBy(_.name)) {
            for (c <- i.getConstraints)
                if (!c.collectDistinctFeatures.exists(f => f.startsWith("CHOICE") || f.endsWith("_MODULE") || f == "MODULES"))
                    if (!c.isTautology())
                        if (!c.isTautology(fm)) {
                            print("#" + i.name)
                            println(" missing constraint: " + c)
                        }
            //                        else println("found: " + c)
        }
    }


    //        def createFromDimacsFile_2Var(file: URI): SATFeatureModel = createFromDimacsFile_2Var_(scala.io.Source.fromFile(file))

    def createFromDimacsFile_2Var(file: String): SATFeatureModel = createFromDimacsFile_2Var_(scala.io.Source.fromFile(file))

    def createFromDimacsFile_2Var_(source: scala.io.Source): SATFeatureModel = {
        var variables: Map[String, Int] = Map()
        val clauses = new Vec[IVecInt]()
        var maxId = 0

        for (line <- source.getLines) {
            if (line startsWith "c ") {
                val entries = line.substring(2).split(" ")
                val id = if (entries(0) endsWith "$")
                    entries(0).substring(0, entries(0).length - 1).toInt
                else
                    entries(0).toInt
                maxId = scala.math.max(id, maxId)
                val varname = if (entries(1).endsWith("_m")) entries(1).substring(0, entries(1).length - 2) + "_MODULE" else entries(1)
                if (variables contains varname)
                    assert(false, "variable " + varname + " declared twice")
                variables = variables.updated(varname, id)
            } else if ((line startsWith "p ") || (line.trim.size == 0)) {
                //comment, do nothing
            } else {
                val vec = new VecInt()
                for (literal <- line.split(" "))
                    if (literal != "0")
                        vec.push(literal.toInt)
                clauses.push(vec)
            }


        }
        assert(maxId == variables.size, "largest variable id " + maxId + " differs from number of variables " + variables.size)
        new SATFeatureModel(variables, clauses, maxId)
    }

}
