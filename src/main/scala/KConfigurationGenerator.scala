package de.fosd.typechef.kconfig

import java.io.File

import de.fosd.typechef.featureexpr.FeatureExprParser


/**
 * This tool creates a valid .config file that satisfies a given formula
 *
 * It needs the following information as input:
 * * The formula to be checked (in a file)
 * * The RSF file created by dumpconf\
 * * path to the kconfig conf tool
 * * The path to the original kconfig file (so we can run the linux tools)
 * * The file to be created
 *
 * It checks the generated formula with the linux configurator tool first.
 *
 *
 */
object KConfigurationGenerator extends App with DifferentialTesting {


    val usage = """
    Usage: KConfigurationGenerator formulaFile rsfFile confTool pathToKconfigFile out
                """

    if (args.length < 5) {
        println(usage);
        sys.exit(1)
    }


    val formulaFile = new File(args(0))
    assert(formulaFile.exists(), "formula %s file not found".format(formulaFile))
    val formula = scala.io.Source.fromFile(formulaFile).getLines().map(_.replace("CONFIG_", "")).map(new FeatureExprParser().parse).reduce(_ and _)

    println("generating configuration for " + formula)

    val rsfFile = new File(args(1))
    assert(rsfFile.exists(), "rsf file %s file not found".format(rsfFile))

    val confToolFile = new File(args(2))
    assert(confToolFile.exists(), "conf tool %s not found".format(confToolFile))

    val kconfigFile = new File(args(3))
    assert(kconfigFile.exists(), "kconfig file %s file not found".format(kconfigFile))

    val outputFile = new File(args(4))

    override def configTool = confToolFile.getAbsolutePath


    println("reading model")
    val model = new XMLDumpReader().readRSF(rsfFile)

    for (feature <- formula.collectDistinctFeatures)
        assert(model.hasItem(if (feature endsWith "_MODULE") feature.dropRight(7) else feature), "literal %s is not known in the model".format(feature))


    for (constr <- model.getConstraints)
        assert((constr and formula).isSatisfiable(), "the formula is not satisfiable in the model constraint "+constr)
    assert((model.getFM and formula).isSatisfiable(), "the formula is not satisfiable in the model")


    val r = (model.getFM and formula).getSatisfiableAssignment(null, model.getAllSymbols, true)
    assert(r.isDefined, "could not find assignment for formula")

    val evalResult = formula.evaluate(r.get._1.map(_.feature).toSet)
    assert(evalResult, "formula does not evaluate to true with found assignment: " + r.get._1.map(_.feature))

    val config = satAssignmentToConfig(r, model)


    val isValid = isValidConfig(kconfigFile.getPath, new File("."), config)

    if (!isValid)
        println("could not generate a valid configuration!")
    else
        new File(".config").renameTo(outputFile)


}
