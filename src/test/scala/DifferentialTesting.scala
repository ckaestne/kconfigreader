package de.fosd.typechef.kconfig

import org.junit._
import java.io._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import FeatureExprFactory._
import scala._
import scala.sys.process.Process

/**
 * differential testing infrastructure and tests for kconfig
 *
 * analyzes all configurations of all features (or a subset of features)
 * in a kconfig file
 *
 * it computes the answer based on the model derived from the kconfig
 * file and checks that answer against running kconfig (the conf tool
 * with --olddefconfig option) which would fix invalid inputs.
 *
 * the kconfig extraction is broken if there is at least one configuration
 * for which kconfig returns a different value than the derived
 * model
 *
 */
trait DifferentialTesting {

    //may be overwritten by specific mixins for specific execution environments
    def dumpconfTool = "/home/energy/undertaker/scripts/kconfig/dumpconf %s > %s"
    def linuxTreeRoot = "/home/energy/linux/"
    def configTool =  "/home/energy/kernel/linux-3.11/scripts/kconfig/conf --olddefconfig %s"


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


    def getModel(workingDir: File, kconfigFile: String, rsfFile:File = null) = {
        assert(workingDir.exists(), "working directory does not exist")
        assert(new File(workingDir, kconfigFile).exists(), "kconfig file does not exist")
        val rsf = if (rsfFile == null) new File(workingDir,  "tmp.rsf") else rsfFile
        rsf.createNewFile()
        Process(dumpconfTool.format(kconfigFile, rsfFile), workingDir).#>(rsf).!
        new RSFReader().readRSF(rsf)
    }


    //    def genExhaustiveCombinations(kconfigFile: String, workingDir: File, fm: KConfigModel) {
    //        genExhaustiveCombinations(kconfigFile, workingDir, fm, fm.items.keys)
    //    }

    def genAllCombinations(kconfigFile: String, workingDir: File, fm: KConfigModel) {
        val configs = explodeConfigs(cleanAssignment(fm.getBooleanSymbols.toList.sorted))

        val result: List[(String, Boolean /*expectedValid*/ , Boolean /*correctResult*/ )] = for (config <- configs) yield {
            val partialAssignment = getPartialAssignment(fm.getBooleanSymbols, config.toSet)
            val isSat = (fm.getFM and partialAssignment).isSatisfiable
            val nonBoolean = getNonBoolean(fm, config.toSet)
            val isValid = isValidConfig(kconfigFile, workingDir, config.toSet, fm.getBooleanSymbols -- config, nonBoolean)
            ((config.sorted ++ nonBoolean.map(v => v._1 + "=" + v._2)).mkString(", "), isSat, isValid == isSat)
        }

        System.err.flush()
        System.out.flush()
        println("!!!!!!!!!!!!!!!!!!")
        result.map(r =>
            if (r._3)
                println(r._1 + " => " + r._2)
            else
                System.err.println(r._1 + " => " + (!r._2) + " by executing kconfig (inferred model states " + r._2 + ")")
        )
        System.err.flush()
        System.out.flush()
        assert(!result.exists(!_._3), "found configuration inconsistency")
    }


    def genAllCombinationsFromPartial(kconfigFile: String, workingDir: File, fm: KConfigModel, featureSet: Set[String]) {

        val configs = explodeConfigs(cleanAssignment(featureSet))

        for (config <- configs) {
            val partialAssignment = getPartialAssignment(featureSet, config.toSet)
            val isSat = (fm.getFM and partialAssignment).isSatisfiable
            val completedConf = if (isSat) {
                genValidAssignment(kconfigFile, workingDir, fm, partialAssignment)
            } else {
                genInvalidAssignment(kconfigFile, workingDir, fm, partialAssignment)
            }
            val nonBoolean = getNonBoolean(fm, completedConf)

            assert(isValidConfig(kconfigFile, workingDir, completedConf, fm.getBooleanSymbols -- completedConf, nonBoolean) == isSat, "expected but did not find " + isSat)
        }


    }

    def getNonBoolean(fm: KConfigModel, assignedValues: Set[String]): Map[String, String] = {
        var result = Map[String, String]()
        for ((item, defaults) <- fm.getNonBooleanDefaults) {

            val default = defaults.filter(_._2.eval(assignedValues)).map(_._1).reverse.headOption


            if (default.isDefined)
                result += (item.name -> default.get)
        }
        result
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
        val r = (fm.getFM and partialAssignment).getSatisfiableAssignment(null, fm.getBooleanSymbols.map(createDefinedExternal(_)), true)
        cleanAssignment(r.get._1.map(_.feature).toSet)
    }

    def genInvalidAssignment(kconfigFile: String, workingDir: File, fm: KConfigModel, partialAssignment: FeatureExpr): Set[String] = {
        //get any assignment for the rest and overwrite the given variables
        //this will not always find out whether the assignment is actually permissable
        // (it may be permissable with another base assignment for the other options), but we can try
        val r = fm.getFM.getSatisfiableAssignment(null, fm.getBooleanSymbols.map(createDefinedExternal(_)), true)

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
    private def cleanAssignment(l: List[String]): List[String] =
        l.filterNot(_.startsWith("CHOICE_"))


    private def explodeConfigs(features: Iterable[String]): List[List[String]] =
        if (features.isEmpty) List(Nil)
        else {
            val r = explodeConfigs(features.tail)
            val f = features.head
            (r.map(f :: _)) ++ r
        }


    def isValidConfig(kconfigFile: String, workingDir: File, selected: Set[String], deselected: Set[String], nonBoolean: Map[String, String]): Boolean =
        isValidConfig_(kconfigFile, workingDir, cleanAssignment(selected), cleanAssignment(deselected), nonBoolean)


    private def isValidConfig_(kconfigFile: String, workingDir: File, selected: Set[String], deselected: Set[String], nonBoolean: Map[String, String]): Boolean = {
        println("=============")
        println("checking config: " + (selected.toList.sorted ++ nonBoolean.map(v => v._1 + "=" + v._2)).mkString(", ")) //+ deselected.toList.sorted.map("!" + _).mkString(" (and ", ", ", ")"))

        val configFile = new File(workingDir, ".config")
        val writer = new FileWriter(configFile)
        for (f <- selected)
            if (f endsWith "_MODULE")
                writer.write("CONFIG_%s=m\n".format(f.dropRight(7)))
            else
                writer.write("CONFIG_%s=y\n".format(f))
        for (f <- deselected)
            if (!(f endsWith "_MODULE") && !selected.contains(f + "_MODULE"))
                writer.write("# CONFIG_%s is not set\n".format(f))
        for ((option, value) <- nonBoolean)
            writer.write("CONFIG_%s=%s\n".format(option, value))
        writer.close()

        println(Process(configTool.format(kconfigFile), workingDir).!!)

        var setConfigs: List[String] = Nil
        var setNonBoolean: Map[String, String] = Map()
        val EnabledConfig = "^CONFIG_([a-zA-Z0-9_]+)=y$".r
        val ModuleConfig = "^CONFIG_([a-zA-Z0-9_]+)=m$".r
        val NonBoolean = "^CONFIG_([a-zA-Z0-9_]+)=(\\d+)$".r
        val NonBooleanHex = "^CONFIG_([a-zA-Z0-9_]+)=(0x\\d+)$".r
        val NonBooleanStr = "^CONFIG_([a-zA-Z0-9_]+)=(\".*\")$".r
        for (l <- io.Source.fromFile(configFile).getLines()) {
            l match {
                case EnabledConfig(c) => setConfigs ::= c
                case ModuleConfig(c) => setConfigs ::= c + "_MODULE"
                case NonBoolean(c, v) => setNonBoolean += (c -> v)
                case NonBooleanHex(c, v) => setNonBoolean += (c -> v)
                case NonBooleanStr(c, v) => setNonBoolean += (c -> v)
                case _ =>
            }
        }

        println("found config: " + (setConfigs ++ setNonBoolean.map(v => v._1 + "=" + v._2)).mkString(", "))

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
        for ((k, v) <- nonBoolean)
            if (setNonBoolean.getOrElse(k, "<does not exist>") != v) {
                val msg = "config changed nonboolean value %s from %s to %s ".format(k, v, setNonBoolean.getOrElse(k, "<does not exist>"))
                println(msg)
                return false
            }
        for ((k, v) <- setNonBoolean)
            if (!nonBoolean.contains(k)) {
                val msg = "config introduces additional nonboolean value %s=%s".format(k, v)
                println(msg)
                return false
            }

        return true
    }

}
