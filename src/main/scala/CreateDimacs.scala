package de.fosd.typechef.busybox

import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureExprParser}
import de.fosd.typechef.featureexpr.sat.{SATFeatureModel, SATFeatureExpr}
import java.io.{OutputStreamWriter, PrintStream, File, FileWriter}
import FeatureExprFactory.sat._

/**
 * reads a feature expression from a file (parameter 1) and creates a dimacs file (parameter 2)
 */

class DimacsWriter  {

    def writeAsDimacs(fexpr: SATFeatureExpr, outputFilename: File) {

        val isCNF = false


        val fm = SATFeatureModel.create(if (isCNF) fexpr else fexpr.toCnfEquiSat()).asInstanceOf[SATFeatureModel]

        val out = //new OutputStreamWriter())
            new FileWriter(outputFilename)

        for ((v, i) <- fm.variables)
            out.write("c " + i + " " + (if (v.startsWith("CONFIG_")) v.drop(7) else "$" + v) + "\n")

        out.write("p cnf " + fm.variables.size + " " + fm.clauses.size() + "\n")

        var i = 0
        while (i < fm.clauses.size) {
            val c = fm.clauses.get(i)
            val vi = c.iterator()
            while (vi.hasNext)
                out.write(vi.next + " ")
            out.write("0\n")
            i = i + 1
        }

        out.close()

    }
}
