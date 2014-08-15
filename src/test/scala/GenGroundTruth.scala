package de.fosd.typechef.kconfig


import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import java.io.{FileWriter, File}
import FeatureExprFactory.False

/**
 * simple tool that computes a propositional formula
 * representing all valid configurations of all files in the resource
 * directory by execution Kconfig in a brute-force fashion
 *
 * this is not part of the test suite but can produce ground
 * truth for those models to compare in other tests
 *
 * it creates .formula files in which the feature model is
 * defined (usually not in a very compact form, due to the brute-force
 * extraction process)
 */
object GenGroundTruth extends App with DifferentialTesting {

    val folders = List(
        new File("src/test/resources"),
        new File("src/test/resources/undertaker"),
        new File("src/test/resources/lvat"),
        new File("src/test/resources/gen")
    )

    def isConfigFile(file: File) = file.isFile && {
        val parts = file.getName.split("\\.")
        !parts.head.isEmpty && parts.head != "approx" && (Set("fm", "Kconfig", "config", "conf") contains parts.last)
    }


    for (folder <- folders;
         file <- folder.listFiles()
         if (isConfigFile(file))) {
        val model = getModel(folder, file.getName)

        if (model.getItems.size > 12)
            println("model " + file + " contains too many items (" + model.getItems.size + "), not attempting brute force strategy")
        else {
            println(file.getName)

            val configs = explodeConfigs(model.items.values)

            val result: List[FeatureExpr] = for (config <- configs) yield {
                val isValid = isValidConfig(file.getName, folder, config)
                if (isValid) getPartialAssignment(model, config) else False
            }

            val fm = result.foldLeft(False)(_ or _)

            val writer = new FileWriter(new File(folder, file.getName + ".formula"))
            fm.print(writer)
            writer.close()


        }


    }


}
