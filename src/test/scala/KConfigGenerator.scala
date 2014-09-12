//package de.fosd.typechef.kconfig
//
//import java.io._
//import scala._
//import java.util.Random
//import scala.math._
//
///**
// * random generator of kconfig files
// *
// * (does not seem very useful right now)
// */
//object KConfigGenerator {
//
//    val sizeLimit = 6
//
//    val itemNames = List("A", "B", "C", "D", "E", "F")
//    val choiceNames = List("CHOICE_1", "CHOICE_2", "CHOICE_3")
//    val rand = new Random(System.currentTimeMillis());
//
//
//    def main(args: Array[String]) {
//
//        new File("src/test/resources/gen/").mkdir()
//        for (i <- 0 until 100) {
//            val outfile = new PrintStream(new FileOutputStream(new File("src/test/resources/gen/randombool%02d.conf".format(i))))
//            writeKConfig(genKConfig, outfile)
//        }
//
//    }
//
//    def genKConfig: List[Item] = {
//        val model = new KConfigModel()
//
//        def getRandomItem =
//            model.getItem(itemNames(rand.nextInt(itemNames.size)))
//
//
//        def genRandomExpr(compl: Double): Expr = {
//            if (rand.nextDouble() > compl)
//                return YTrue()
//
//            var result: Expr = Name(getRandomItem)
//
//            while (rand.nextDouble() < compl) {
//                val r = rand.nextDouble()
//                if (r < 0.5)
//                    result = And(result, Name(getRandomItem))
//                else if (r < 0.8)
//                    result = Or(result, Name(getRandomItem))
//                else
//                    result = Not(result)
//            }
//
//            result
//
//        }
//
//        var items: List[Item] = Nil
//        //get items in random order
//        while (items.size < min(sizeLimit, itemNames.size)) {
//            val randomItem = getRandomItem
//            if (!(items contains randomItem))
//                items = randomItem :: items
//        }
//
//        //randomly make prompt or not
//        val promptProbability = rand.nextDouble()
//        for (item <- items)
//            item.setPrompt(if (rand.nextDouble() < promptProbability) YTrue() else Not(YTrue()))
//
//        //random defaults
//        for (item <- items)
//            item.setDefault(TristateConstant('y'), genRandomExpr(0.2))
//
//        //random dependencies
//        for (item <- items)
//            item.setDepends(genRandomExpr(0.1))
//
//        //random selects
//        val selectProbability = rand.nextDouble()/2
//        for (item <- items)
//            if (selectProbability < rand.nextDouble())
//                item.setSelectedBy(getRandomItem, genRandomExpr(0.1))
//
//
//        items
//    }
//
//
//    def writeKConfig(items: List[Item], stream: PrintStream = System.out) {
//        for (item <- items) {
//
//            stream.println("config %s".format(item.name))
//            //prompt
//            stream.print("\tbool")
//            if (item.hasPrompt==YTrue())
//                stream.print(" \"prompt %s\"".format(item.name))
//            stream.println("")
//            //depends
//            if (item.depends.isDefined && item.depends.get != YTrue())
//                stream.println("\tdepends on %s".format(item.depends.get.kexpr))
//            //selected by, changed as selects
//            for ((i, cond) <- item.selectedBy) {
//                stream.print("\tselect %s".format(i.name))
//                if (cond != YTrue())
//                    stream.print(" if %s".format(cond.kexpr))
//                stream.println("")
//            }
//            for ((v, cond) <- item.default) {
//                stream.print("\tdefault %s".format(v))
//                if (cond != YTrue())
//                    stream.print(" if %s".format(cond.kexpr))
//                stream.println("")
//            }
//
//            stream.println("")
//
//        }
//
//    }
//
//}
