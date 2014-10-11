package de.fosd.typechef.kconfig


import java.io._

import de.fosd.typechef.featureexpr.FeatureExprFactory._
import de.fosd.typechef.featureexpr.{FeatureExpr, SingleFeatureExpr}

import scala.sys.process.{Process, ProcessLogger}

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
    //the tool paths are relative to the working directory, but $PWD can be used to
    //substitute the absolute path of this project's root
    def dumpconfTool = (sys.env.getOrElse("DUMPCONF", "$PWD/binary/dumpconf") + " %s").replace("$PWD", new File(".").getAbsolutePath)

    def linuxTreeRoot = sys.env.getOrElse("LINUXROOT", "src/test/resources/linux/")

    def configTool = sys.env.getOrElse("CONFTOOL", "$PWD/binary/conf").replace("$PWD", new File(".").getAbsolutePath)
    def configToolOldConfig = configTool + " --olddefconfig %s"


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
        for (item <- model.items.values) {
            println("//" + item.name)
            println(item.getConstraints.mkString("\n"))
        }
        for (item <- model.choices.values) {
            println("//" + item.name + "?")
            println(item.getConstraints.mkString("\n"))
        }
        //        model.items.values.map(i=>println(i.name+": "+i.knownValues.mkString(", ")))

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


    def getModel(workingDir: File, kconfigFile: String, rsfFile: File = null) = {
        assert(workingDir.exists(), "working directory does not exist")
        assert(new File(workingDir, kconfigFile).exists(), "kconfig file does not exist")
        val rsf = if (rsfFile == null) new File(workingDir, "tmp.rsf") else rsfFile
        rsf.createNewFile()
        val cmd = dumpconfTool.format(kconfigFile)
        try {
            Process(cmd, workingDir).#>(rsf).!!(ProcessLogger(f => System.err.println(f)))
        } catch {
            case e: RuntimeException =>
                System.err.println("failed executing %s in %s".format(cmd, workingDir))
                throw e
        }
        Thread.sleep(100)
        new XMLDumpReader().readRSF(rsf)
    }


    //    def genExhaustiveCombinations(kconfigFile: String, workingDir: File, fm: KConfigModel) {
    //        genExhaustiveCombinations(kconfigFile, workingDir, fm, fm.items.keys)
    //    }

    def genAllCombinations(kconfigFile: String, workingDir: File, fm: KConfigModel) {
        val configs = explodeConfigs(fm.items.values.filter(s => !s.isChoice).toList.sortBy(_.name))

        assert(configs.size <= 4096, "Refusing to execute a brute-force comparison against %d configurations; simplify the kconfig file".format(configs.size))

        val result: List[(String, Boolean /*expectedValid*/ , Boolean /*correctResult*/ )] = for (config <- configs) yield {
            val partialAssignment = getPartialAssignment(fm, config)
            val isSat = (fm.getFM and partialAssignment).isSatisfiable
            val isValid = isValidConfig(kconfigFile, workingDir, config)
            (printConfig(config), isSat, isValid == isSat)
        }

        checkResult(result)
    }

    def checkResult(result: List[(String, Boolean, Boolean)]) {
        System.err.flush()
        System.out.flush()
        println("!!!!!!!!!!!!!!!!!!")
        var lastError: String = ""
        result.map(r =>
            if (r._3)
                println(r._1 + " => " + r._2)
            else {
                lastError = r._1 + " => " + (!r._2) + " by executing kconfig (inferred model states " + r._2 + ")"
                System.err.println(lastError)
            }
        )
        System.err.flush()
        System.out.flush()
        assert(!result.exists(!_._3), "found configuration inconsistency: " + lastError)
    }

    private def printConfig(config: Map[String, String]): String = {
        config.filter(_._2 != "n").toList.sortBy(_._1).map(formatConfig).mkString(", ")
    }

    private def formatConfig(v: (String, String)): String = {
        if (v._2 == "y") v._1
        else if (v._2 == "m") v._1 + "_MODULE"
        else v._1 + "=" + v._2
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


    def getPartialAssignment(fm: KConfigModel, config: Map[String, String]): FeatureExpr = {
        var expr: FeatureExpr = True

        for ((feature, value) <- config) {

            val item = fm.findItem(feature)

            if (item.isChoice) {}
            else if (item.isTristate) {
                if (value == "y") expr = expr and item.fexpr_y andNot item.fexpr_m
                else if (value == "m") expr = expr andNot item.fexpr_y and item.fexpr_m
                else expr = expr andNot item.fexpr_y andNot item.fexpr_m
            } else if (!item.isNonBoolean) {
                if (value == "y") expr = expr and item.fexpr_y
                else expr = expr andNot item.fexpr_y
            } else {
                //nonboolean
                val v = if (item._type == StringType && value != "n") value.drop(1).dropRight(1) else value //get rid of the quotes for strings
                expr = expr and item.getNonBooleanValue(v)
            }
        }

        expr
    }


    //    def checkConfig_(kconfigFile: String, workingDir: File, r: Option[Pair[List[SingleFeatureExpr], List[SingleFeatureExpr]]]) {
    //        assert(r.isDefined, "SAT solver did not find assignment")
    //
    //        val selected = cleanAssignment(r.get._1)
    //        val deselected = cleanAssignment(r.get._2)
    //
    //        checkConfig(kconfigFile, workingDir, selected, deselected)
    //    }

    //    private def cleanAssignment(l: Set[String], model: KConfigModel): Set[String] =
    //        cleanAssignment(l.toList, model).toSet


    protected def explodeConfigs(features: Iterable[Item]): List[Map[String, String]] =
        if (features.isEmpty) List(Map())
        else {
            val r = explodeConfigs(features.tail)
            val f = features.head
            var values =
                if (f.isTristate) List("y", "m", "n")
                else if (!f.isNonBoolean) List("y", "n")
                else /*nonboolean*/ f.knownNonBooleanValues.toList
            if (f._type == StringType)
                values = values.map("\"" + _ + "\"")
            if (f.isNonBoolean)
                values ::= "n"
            assert(!values.isEmpty)

            values.flatMap(value =>
                r.map(_ + (f.name -> value)))
        }


    protected def writeConfigFile(config: Map[String, String], writer: Writer): Unit = {
        for ((k, v) <- config)
            if (Set("y", "m") contains v)
                writer.write("CONFIG_%s=%s\n".format(k, v))
            else if (v == "n")
                writer.write("# CONFIG_%s is not set\n".format(k))
            else
                writer.write("CONFIG_%s=%s\n".format(k, v))
    }

    protected def isValidConfig(kconfigFile: String, workingDir: File, config: Map[String, String]): Boolean = {
        println("=============")
        println("checking config: " + printConfig(config))

        val configFile = new File(workingDir, ".config")
        val writer = new FileWriter(configFile)
        writeConfigFile(config, writer)
        writer.close()

        assert(Process(configToolOldConfig.format(kconfigFile), workingDir, ("ARCH", "x86"), ("KERNELVERSION", "3.11")).! == 0, "error executing " + configToolOldConfig.format(kconfigFile))

        val foundConfig: Map[String, String] = readConfigFile(configFile)

        println("found config: " + printConfig(foundConfig))

        val blacklist = Set("ARCH", "KERNELVERSION") //special features, ignored here
        var foundProblem = false
        for ((k, v) <- config; if !(blacklist contains k))
            if (foundConfig.getOrElse(k, "n") != v) {
                val msg = "kconfig changed %s from '%s' to '%s'".format(k, v, foundConfig.getOrElse(k, "n"))
                println(msg)
                foundProblem = true
            }
        for ((k, v) <- foundConfig)
            if (!config.contains(k)) {
                val msg = "kconfig introduces %s='%s'".format(k, v)
                println(msg)
                foundProblem = true
            }

        return !foundProblem
    }


    def readConfigFile(configFile: File): Map[String, String] = {
        var foundConfig: Map[String, String] = Map()
        val EnabledConfig = "^CONFIG_([a-zA-Z0-9_]+)=y$".r
        val ModuleConfig = "^CONFIG_([a-zA-Z0-9_]+)=m$".r
        val NonBoolean = "^CONFIG_([a-zA-Z0-9_]+)=(-?\\d+)$".r
        val NonBooleanHex = "^CONFIG_([a-zA-Z0-9_]+)=(0?x?[A-Fa-f0-9]+)$".r
        val NonBooleanStr = "^CONFIG_([a-zA-Z0-9_]+)=(\".*\")$".r
        for (l <- io.Source.fromFile(configFile).getLines() if !(l.startsWith("#")) if (!l.trim.isEmpty)) {
            l match {
                case EnabledConfig(c) => foundConfig += (c -> "y")
                case ModuleConfig(c) => foundConfig += (c -> "m")
                case NonBoolean(c, v) => foundConfig += (c -> v)
                case NonBooleanHex(c, v) => foundConfig += (c -> v)
                case NonBooleanStr(c, v) => foundConfig += (c -> v)
                case _ => println("unmatched .config line " + l)
            }
        }
        foundConfig
    }

    /** ************
      * partial
      */

    def genAllCombinationsFromPartial(kconfigFile: String, workingDir: File, fm: KConfigModel, featureSet: Set[String], minimizeConfigurations: Boolean = true) {
        def cleanAssignment(l: Set[String], model: KConfigModel): Set[String] =
            l.filterNot(fm.findItem(_).isChoice).filter(s => s.endsWith("_MODULE") || model.findItem(s).isDefined)

        val configs = explodeConfigs(cleanAssignment(featureSet, fm).map(fm.findItem))

        val result: List[(String, Boolean /*expectedValid*/ , Boolean /*correctResult*/ )] = for (config <- configs) yield {
            val partialAssignment = getPartialAssignment(fm, config)
            val isSat = (fm.getFM and partialAssignment).isSatisfiable
            val completedConf = if (isSat) {
                genValidAssignment(fm, partialAssignment, minimizeConfigurations)
            } else {
                genInvalidAssignment(fm, partialAssignment, minimizeConfigurations)
            }

            val isValid = isValidConfig(kconfigFile, workingDir, completedConf)
            assert(isValid == isSat, "expected %s, kconfig %s".format(if (isSat) "valid" else "invalid", if (isValid) "accepted assignment" else "changed assignment"))
            (printConfig(config), isSat, isValid == isSat)
        }

        checkResult(result)

    }


    def genValidAssignment(fm: KConfigModel, partialAssignment: FeatureExpr, minimizeConfigurations: Boolean = true): Map[String, String] = {
        val r = (fm.getFM and partialAssignment).getSatisfiableAssignment(null, fm.getAllSymbols, minimizeConfigurations)
        satAssignmentToConfig(r, fm)
    }

    def genInvalidAssignment(fm: KConfigModel, partialAssignment: FeatureExpr, minimizeConfigurations: Boolean = true): Map[String, String] = {
        //get any assignment for the rest and overwrite the given variables
        //this will not always find out whether the assignment is actually permissable
        // (it may be permissable with another base assignment for the other options), but we can try
        var r = fm.getFM.getSatisfiableAssignment(null, fm.getAllSymbols, minimizeConfigurations)
        assert(r.isDefined)
        var selected = r.get._1.toSet
        var deselected = r.get._2.toSet

        for (f <- partialAssignment.collectDistinctFeatureObjects)
            if ((partialAssignment and f).isSatisfiable) {
                selected += f
                deselected -= f
            } else {
                selected -= f
                deselected += f
            }

        satAssignmentToConfig(Some((selected.toList, deselected.toList)), fm)

    }


    protected def satAssignmentToConfig(r: Option[(List[SingleFeatureExpr], List[SingleFeatureExpr])], fm: KConfigModel): Map[String, String] = {
        assert(r.isDefined)

        val (enabled, disabled) = r.get

        var result: Map[String, String] = Map()
        for (f <- disabled) {
            if (fm.hasItem(f.feature) && fm.findItem(f.feature).isChoice) {
                /*nothing*/
            }
            else if (f.feature.endsWith("_MODULE"))
                result += (f.feature.dropRight(7) -> "n")
            else if (f.feature contains "=")
                result += (f.feature.take(f.feature.indexOf("=")) -> "n")
            else
                result += (f.feature -> "n")
        }
        for (f <- enabled) {
            if (fm.hasItem(f.feature) && fm.findItem(f.feature).isChoice) {
                /*nothing*/
            }
            else if (f.feature.endsWith("_MODULE"))
                result += (f.feature.dropRight(7) -> "m")
            else if (f.feature contains "=") {
                val k = f.feature.take(f.feature.indexOf("="))
                var v = f.feature.substring(f.feature.indexOf("=") + 1)
                if (fm.findItem(k)._type == StringType && v != "n")
                    v = "\"" + v + "\""
                result += (k -> v)
            } else
                result += (f.feature -> "y")
        }
        result
    }

}
