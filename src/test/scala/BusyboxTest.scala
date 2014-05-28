package de.fosd.typechef.kconfig

import org.junit._
import java.io._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import FeatureExprFactory._
import scala._
import scala.sys.process.Process

/**
 * Created by ckaestne on 5/23/14.
 */
class BusyboxTest {

    val dumpconfTool = "/usr/lib/undertaker/dumpconf %s > %s"
    val linuxTreeRoot = "/usr0/home/ckaestne/work/TypeChef/LinuxAnalysis/gitlinux/"
    val configTool = linuxTreeRoot + "scripts/kconfig/conf --olddefconfig %s"


    @Test
    def testBusybox() {
        val workingDir = new File("/usr0/home/ckaestne/work/TypeChef/BusyboxAnalysis/gitbusybox/")
        val kconfigFile = "Config.in"
        val model = getModel(workingDir, kconfigFile)
        genAllCombinationsFromPartial(kconfigFile, workingDir, model, Set("FEATURE_CHECK_UNICODE_IN_ENV", "UNICODE_SUPPORT", "UNICODE_USING_LOCALE"))
    }
                                         d

    @Test def testMiniKConfig() {
        checkTestModelBruteForce("src/test/resources/mini.config")
    }
    @Test def testMini2KConfig() {
        checkTestModelBruteForce("src/test/resources/mini2.config")
    }
    @Test def testMini3KConfig() {
        checkTestModelBruteForce("src/test/resources/mini3.config")
    }

    @Test def testDepKConfig() {
        checkTestModelBruteForce("src/test/resources/dep.config")
    }

    @Test def testSelKConfig() {
        checkTestModelBruteForce("src/test/resources/sel.config")
    }

    @Test def testSelHiddenKConfig() {
        checkTestModelBruteForce("src/test/resources/selhidden.config")
    }


    @Test def testSelHidden2KConfig() {
        checkTestModelBruteForce("src/test/resources/selhidden2.config")
    }
    @Test def testSelHidden3KConfig() {
        checkTestModelBruteForce("src/test/resources/selhidden3.config")
    }

    @Test def testSelHidden4KConfig() {
        checkTestModelBruteForce("src/test/resources/selhidden4.config")
    }
    @Test def testSelHidden5KConfig() {
        checkTestModelBruteForce("src/test/resources/selhidden5.config")
    }
    @Test def testChoiceKConfig() {
        checkTestModelBruteForce("src/test/resources/choice.config")
    }

//    private def printAllConfig(kconfigFile: String) {
//        val workingDir = new File(".")
//        val fm = getModel(workingDir, kconfigFile)
//
//        val configs = explodeConfigs(fm.getItems)
//
//        val results = for (config <- configs) yield
//             isValidConfig(kconfigFile, workingDir, config.toSet, fm.getItems -- config)
//
//        println((configs zip results).map(l => l._1.mkString(", ") + " => " + l._2).mkString("\n"))
//
//    }

    /**
     * checks the read Kconfig abstraction against all configurations checked
     * by running the linux/conf tool on the actual configurations.
     * this is a form of differential testing where linux/conf provides the
     * oracle
     *
     * tries all combinations in a brute force fashion, so use only for
     * small kconfig models
     */
    def checkTestModelBruteForce(kconfigFile: String) {
        val workingDir = new File(".")
        val model = getModel(workingDir, kconfigFile)

        println("**********\n" +
            "** " + kconfigFile)
        println(model.getConstraints.mkString("\n"))

        genAllCombinations(kconfigFile, workingDir, model)
    }


    /**
     * this checks a subset of features in a larger model.
     *
     * for every combination of features in the subset it will create a full configuration
     * and run it against the linux/conf configurator.
     *
     * this will detect when the inferred model is not strict enough, i.e.,
     * valid configurations are not supposed to be valid according to kconfig.
     * a valid configuration is completed from a partial configuration (of the subset)
     * with a SAT solver
     *
     * this will however not necessarily detect if a configuration is too strict.
     * this test will attempt to create an invalid configuration, but there
     * is no guarantee that it will actually find a feature assignment
     * for features outside the subset.
     */
    def checkTestModel(kconfigFile: String, workingDir: File,
                       features: Set[String]) {

        val model = getModel(workingDir, kconfigFile)

        println("**********\n" +
            "** " + kconfigFile)
        println(model.getConstraints.mkString("\n"))

        genAllCombinationsFromPartial(kconfigFile, workingDir, model, features)
    }


    def getModel(workingDir: File, kconfigFile: String) = {
        assert(workingDir.exists(), "working directory does not exist")
        assert(new File(workingDir, kconfigFile).exists(), "kconfig file does not exist")
        val rsfFile = "tmp.rsf"
        val rsf = new File(workingDir, rsfFile)
        rsf.createNewFile()
        Process(dumpconfTool.format(kconfigFile, rsfFile), workingDir).#>(rsf).!
        new RSFReader().readRSF(rsf)
    }


    //    def genExhaustiveCombinations(kconfigFile: String, workingDir: File, fm: KConfigModel) {
    //        genExhaustiveCombinations(kconfigFile, workingDir, fm, fm.items.keys)
    //    }

    def genAllCombinations(kconfigFile: String, workingDir: File, fm: KConfigModel) {
        val configs = explodeConfigs(fm.getItems.toList.sorted)

        val result:List[(String, Boolean/*expectedValid*/, Boolean/*correctResult*/)] = for (config <- configs) yield {
            val partialAssignment = getPartialAssignment(fm.getItems, config.toSet)
            val isSat = (fm.getFM and partialAssignment).isSatisfiable
            val isValid = isValidConfig(kconfigFile, workingDir, config.toSet, fm.getItems -- config)
            (config.sorted.mkString(", "),isSat,isValid== isSat)
        }

        System.err.flush()
        System.out.flush()
        println("!!!!!!!!!!!!!!!!!!")
        result.map(r =>
            if (r._3)
                println(r._1+" => "+r._2)
            else
                System.err.println(r._1+" => "+(!r._2)+" by executing kconfig (inferred model states "+r._2+")")
        )
        System.err.flush()
        System.out.flush()
        assert(!result.exists(!_._3), "found configuration inconsistency")
    }


    def genAllCombinationsFromPartial(kconfigFile: String, workingDir: File, fm: KConfigModel, featureSet: Set[String]) {

        val configs = explodeConfigs(featureSet)

        for (config <- configs) {
            val partialAssignment = getPartialAssignment(featureSet, config.toSet)
            val isSat = (fm.getFM and partialAssignment).isSatisfiable
            val completedConf = if (isSat) {
                genValidAssignment(kconfigFile, workingDir, fm, partialAssignment)
            } else {
                genInvalidAssignment(kconfigFile, workingDir, fm, partialAssignment)
            }

            assert(isValidConfig(kconfigFile, workingDir, completedConf, fm.getItems -- completedConf) == isSat, "expected but did not find "+isSat)
        }


    }


    //    def checkValidAssignment(kconfigFile: String, workingDir: File, fm: KConfigModel, assignment: FeatureExpr) {
    //        checkConfig_(kconfigFile, workingDir, (fm.getFM and assignment).getSatisfiableAssignment(null, fm.getItems, true))
    //    }


    def getPartialAssignment(featureSet: Set[String], selection: Set[String]): FeatureExpr = {
        var expr: FeatureExpr = True
        for (f <- selection)
            expr = expr and createDefinedExternal(f)
        for (f <- (featureSet -- selection))
            expr = expr andNot createDefinedExternal(f)
        expr
    }

    def genValidAssignment(kconfigFile: String, workingDir: File, fm: KConfigModel, partialAssignment: FeatureExpr): Set[String] = {
        val r = (fm.getFM and partialAssignment).getSatisfiableAssignment(null, fm.getItems.map(createDefinedExternal(_)), true)
        cleanAssignment(r.get._1.map(_.feature).toSet)
    }

    def genInvalidAssignment(kconfigFile: String, workingDir: File, fm: KConfigModel, partialAssignment: FeatureExpr): Set[String] = {
        //get any assignment for the rest and overwrite the given variables
        //this will not always find out whether the assignment is actually permissable
        // (it may be permissable with another base assignment for the other options), but we can try
        val r = fm.getFM.getSatisfiableAssignment(null, fm.getItems.map(createDefinedExternal(_)), true)

        assert(r.isDefined, "SAT solver did not find assignment at all")

        var selected = cleanAssignment(r.get._1.map(_.feature).toSet)
        //        var deselected = cleanAssignment(r.get._2.map(_.feature).toSet)

        for (f <- partialAssignment.collectDistinctFeatureObjects)
            if ((partialAssignment and f).isSatisfiable) {
                selected = selected + f.feature
                //                deselected = deselected - f.feature
            } else {
                selected = selected - f.feature
                //                deselected = deselected + f.feature
            }


        selected
    }

    //    def checkConfig_(kconfigFile: String, workingDir: File, r: Option[Pair[List[SingleFeatureExpr], List[SingleFeatureExpr]]]) {
    //        assert(r.isDefined, "SAT solver did not find assignment")
    //
    //        val selected = cleanAssignment(r.get._1)
    //        val deselected = cleanAssignment(r.get._2)
    //
    //        checkConfig(kconfigFile, workingDir, selected, deselected)
    //    }

    private def cleanAssignment(l: Set[String]): Set[String] =
        l.filterNot(_.startsWith("CHOICE_"))


    private def explodeConfigs(features: Iterable[String]): List[List[String]] =
        if (features.isEmpty) List(Nil)
        else {
            val r = explodeConfigs(features.tail)
            val f = features.head
            (r.map(f :: _)) ++ r
        }


    def isValidConfig(kconfigFile: String, workingDir: File, selected: Set[String], deselected: Set[String]): Boolean =
        isValidConfig_(kconfigFile, workingDir, cleanAssignment(selected), cleanAssignment(deselected))


    private def isValidConfig_(kconfigFile: String, workingDir: File, selected: Set[String], deselected: Set[String]): Boolean = {
        println("=============")
        println("checking config: " + selected.toList.sorted.mkString(", ")) //+ deselected.toList.sorted.map("!" + _).mkString(" (and ", ", ", ")"))

        val configFile = new File(workingDir, ".config")
        val writer = new FileWriter(configFile)
        for (f <- selected)
            writer.write("CONFIG_%s=y\n".format(f))
        for (f <- deselected)
            writer.write("# CONFIG_%s is not set\n".format(f))
        writer.close()

        println(Process(configTool.format(kconfigFile), workingDir).!!)

        var setConfigs: List[String] = Nil
        for (l <- io.Source.fromFile(configFile).getLines()) {
            if (l.startsWith("CONFIG_") && l.endsWith("=y"))
                setConfigs ::= l.dropRight(2).drop(7)
        }

        println("found config: " + setConfigs.mkString(", "))

        for (s <- selected)
            if (!setConfigs.contains(s)) {
                val msg = "config does not match, kconfig-generated configuration removes " + s
                println(msg)
                return false
            }
        for (s <- deselected)
            if (setConfigs.contains(s)) {
                val msg = "config does not match, kconfig-generated configuration adds " + s
                println(msg)
                return false
            }

        return true
    }

}

class KConfigException(msg: String) extends Exception(msg)
