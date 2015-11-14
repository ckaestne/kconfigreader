package de.fosd.typechef.kconfig.tools

import java.io.{BufferedWriter, File, FileWriter}

import de.fosd.typechef.featureexpr._

import scala.io.Source


/**
 * tool that takes multiple dimacs files created by kconfig reader and merges them
 * using a top level choice.
 *
 * the idea is to merge separate models for different architectures into a single model
 * in which one can select among architectures
 *
 * takes .dimacs files and flags that describe the feature that guards their inclusion;
 * adds a constraint that exactly one of the features needs to be selected;
 * each of the previous constraints is then guarded by that architecture
 */
object KConfigMerger extends App {

    val usage = """
    Usage: kconfigmerger --dimacs <file> <flag> --dimacs <file> <flag> out
                """

    if (args.length == 0) {
        println(usage);
        sys.exit(1)
    }
    val arglist = args.toList
    type OptionMap = Map[String, String]
    var dimacsFiles = List[(File, SingleFeatureExpr)]()

    //simple option parser
    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
        def isSwitch(s: String) = (s(0) == '-')
        list match {
            case Nil => map
            case "--dimacs" :: file :: flag :: tail =>
                dimacsFiles ::=(new File(file), FeatureExprFactory.createDefinedExternal(flag)) //dirty
                nextOption(map, tail)
            case string :: Nil if !isSwitch(string) => nextOption(map ++ Map("out" -> string), Nil)
            case option :: tail => println("Unknown option " + option)
                println(map);
                sys.exit(1)
        }
    }

    val options = nextOption(Map(), arglist)


    val out = new File(options("out"))
    assert(dimacsFiles.size > 1, "need at least two dimacs files")
    dimacsFiles.map(f => assert(f._1.exists, "dimacs file does not exist: " + f._1))

    var globalVariableMap = Map[String, Int]()
    var globalVariableCounter = 0


    //exactly one architecture needs to be selected
    val archFM = genArchConstraints(for (d <- dimacsFiles) yield getGlobalId(d._2.feature))
    //    println(archFM)


    val featureModels = for ((dimacsFile, feature) <- dimacsFiles) yield {
	println("loading "+feature.feature+" in "+dimacsFile)
        val archFeatureId = getGlobalId(feature.feature)
        //add extra constraint on ARCH feature
        val fm = loadDimacsData(Source.fromFile(dimacsFile)).map((-archFeatureId) :: _)

        //        println(fm)
        fm
    }
    //    println(globalVariableMap)


    println("writing result in "+out)
    writeAsDimacs(out, globalVariableMap, (archFM :: featureModels).flatten)


    protected def loadDimacsData(file: Source): List[List[Int]] = {
        var localVariableMap: Map[String, Int] = Map()
        var localToGlobalMap: Map[Int, Int] = Map()
        var clauses = List[List[Int]]()

        var numDeclaredVariables = -1
        var numDeclaredClauses = -1

        def localToGlobalClause(vec: List[Int]): List[Int] =
            vec.map(v => if (v < 0) -localToGlobal(-v) else localToGlobal(v))
        def localToGlobal(v: Int): Int =
            if (localToGlobalMap.contains(v))
                localToGlobalMap(v)
            else {
                globalVariableCounter += 1
                localToGlobalMap += (v -> globalVariableCounter)
                globalVariableCounter
            }


        for (line <- file.getLines) {
            if (line startsWith "c ") {
                val entries = line.substring(2).split(" ")
                val id = if (entries(0) endsWith "$")
                    entries(0).substring(0, entries(0).length - 1).toInt
                else
                    entries(0).toInt
                val varname = entries(1)
                assert(!(localVariableMap contains varname), "Inconsistent dimacs file: variable " + varname + " declared twice")
                localVariableMap += (varname -> id)
                val globalId = getGlobalId(varname)
                localToGlobalMap += (id -> globalId)
            } else if (line startsWith "p ") {
                val entries = line.split(" ")
                assert(entries(1) == "cnf")
                numDeclaredVariables = entries(2).toInt
                numDeclaredClauses = entries(3).toInt
            } else if (line.trim.size == 0) {
                //comment, do nothing
            } else {
                var vec = List[Int]()
                for (literal <- line.split(" "))
                    if (literal != "0")
                        vec ::= literal.toInt
                clauses ::= localToGlobalClause(vec)
            }

        }
        assert(clauses.size == numDeclaredClauses, "Inconsistent dimacs file: number of clauses %d differes from declared number of clauses %d".format(clauses.size, numDeclaredClauses))
        clauses
    }

    def getGlobalId(varname: String): Int =
        if (globalVariableMap.contains(varname))
            globalVariableMap(varname)
        else {
            globalVariableCounter += 1
            globalVariableMap += (varname -> globalVariableCounter)
            globalVariableCounter
        }


    def genArchConstraints(archs: List[Int]): List[List[Int]] =
        List(archs) ++
            (for ((a, b) <- pairs(archs)) yield List(-a, -b))


    def pairs[A](elem: List[A]): Iterator[(A, A)] =
        for (a <- elem.tails.take(elem.size); b <- a.tail) yield (a.head, b)


    def writeAsDimacs(outputFilename: File, variables: Map[String, Int], clauses: List[List[Int]]) {

        val out = new BufferedWriter(new FileWriter(outputFilename))

        for ((v, i) <- variables)
            out.write("c " + i + " " + v + "\n")

        out.write("p cnf " + globalVariableCounter + " " + clauses.size + "\n")

        for (c <- clauses)
            for (v <- c)
                out.write(v + " ")
        out.write("0\n")


        out.close
    }
}
