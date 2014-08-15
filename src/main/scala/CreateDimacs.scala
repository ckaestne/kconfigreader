package de.fosd.typechef.kconfig

import de.fosd.typechef.featureexpr.SingleFeatureExpr
import de.fosd.typechef.featureexpr.sat._
import java.io.{File, FileWriter}
import org.sat4j.specs.{IVecInt, IVec}


class DimacsWriter {

    def writeAsDimacs(fexpr: SATFeatureExpr, outputFilename: File) {

        val isCNF = false


        val fm = SATFeatureModel.create(if (isCNF) fexpr else fexpr.toCnfEquiSat()).asInstanceOf[SATFeatureModel]


        writeAsDimacsRaw(outputFilename, fm.variables, fm.clauses)
    }

    def writeAsDimacs2(fexprs: List[SATFeatureExpr], outputFilename: File, equisatTransformation: Boolean = true) {
        import de.fosd.typechef.featureexpr.sat._
        import CNFHelper._

        var varIDs: Map[SingleFeatureExpr, Int] = Map()
        var nextID = 1
        val sb = new StringBuilder
        def getId(f: SingleFeatureExpr): Int = {
            if (!(varIDs contains f)) {
                varIDs += (f -> nextID)
                nextID = nextID + 1
            }
            varIDs(f)
        }
        var clauses = 0
        for (fexpr <- fexprs) {
            var cnf = if (equisatTransformation) fexpr.toCnfEquiSat() else fexpr.toCNF()


            for (clause <- getCNFClauses(cnf)) {
                for (lit <- getLiterals(clause))
                    lit match {
                        case d: DefinedExpr => sb.append(getId(d)).append(" ")
                        case Not(d: DefinedExpr) => sb.append("-").append(getId(d)).append(" ")
                        case _ => assert(false, "invalid CNF literal " + lit)
                    }
                sb.append("0\n")
                clauses += 1
            }
        }

        val out = //new OutputStreamWriter())
            new FileWriter(outputFilename)

        for ((f, id) <- varIDs; if !(f.feature startsWith "__fresh"))
            out.write("c " + id + " " + f.feature + "\n")

        out.write("p cnf " + varIDs.size + " " + clauses + "\n")

        out.write(sb.toString())

        out.close()
    }


    def writeAsDimacsRaw(outputFilename: File, variables: Map[String, Int], clauses: IVec[IVecInt]) {

        val out = //new OutputStreamWriter())
            new FileWriter(outputFilename)

        for ((v, i) <- variables)
            out.write("c " + i + " " + (if (v.startsWith("CONFIG_")) v.drop(7) else "$" + v) + "\n")

        out.write("p cnf " + variables.size + " " + clauses.size() + "\n")

        var i = 0
        while (i < clauses.size) {
            val c = clauses.get(i)
            val vi = c.iterator()
            while (vi.hasNext)
                out.write(vi.next + " ")
            out.write("0\n")
            i = i + 1
        }

        out.close()
    }

    //    def bddtest(fexprs: List[SATFeatureExpr], outputFilename: File) {
    //        import de.fosd.typechef.featureexpr.sat._
    //
    ////        val bddf=new BDDFeatureExprHack()
    //
    //        def toBDD(expr: SATFeatureExpr): /*BDD*/FeatureExpr = {
    //            expr match {
    //                case And(clauses) =>
    //                    clauses.map(toBDD).foldLeft(BDDFeatureExprFactory.True)(_ and _)
    //                case Or(clauses) =>
    //                    clauses.map(toBDD).foldLeft(BDDFeatureExprFactory.False)(_ or _)
    //                case Not(e) =>
    //                    toBDD(e).not()
    //                case DefinedExpr(n) => {
    //                    val bdd = BDDFeatureExprFactory.createDefinedExternal(n.feature)
    ////                    BDDFeatureExprHack.inc(bdd)
    //                    bdd
    //                }
    ////                case DefinedExpr(n) => bddf.definedExternal(n.feature)
    //
    //
    //            }
    //        }
    //
    //        for (fexpr <- fexprs) {
    //            println(fexpr)
    //
    //            val bdd=toBDD(fexpr)
    //            println(bdd)
    ////            BDDFeatureExprHack.free(bdd.asInstanceOf[BDDFeatureExpr])
    //
    //            BDDFeatureExprHack.resetBDD
    //        }
    //
    //    }
    //

}

//object Tmp extends App {
//
//    val f= """
//          |(((def(ISDN_I4L)&def(ISDN_DRV_HISAX)&def(EXPERIMENTAL)&def(USB)&def(ISDN))|(def(ISDN)&((def(USB)&def(EXPERIMENTAL)&def(ISDN_DRV_HISAX)&def(ISDN_I4L))|(((def(USB)&def(EXPERIMENTAL)&def(ISDN_DRV_HISAX))|(((def(USB)&def(EXPERIMENTAL))|(def(EXPERIMENTAL)&(def(USB)|def(USB_MODULE))))&(def(ISDN_DRV_HISAX)|def(ISDN_DRV_HISAX_MODULE))))&(def(ISDN_I4L)|def(ISDN_I4L_MODULE))))))&(def(HISAX_ST5481)|def(HISAX_ST5481_MODULE)))
//          ||
//          |(def(ISDN)&(def(ISDN_HDLC)|def(ISDN_HDLC_MODULE)))
//          ||
//          |(((def(FB)&def(PCI)&def(HAS_IOMEM))|(def(HAS_IOMEM)&((def(FB)&def(PCI))|(def(PCI)&(def(FB)|def(FB_MODULE))))))&(def(FB_RIVA)|def(FB_RIVA_MODULE)))
//          ||
//          |!def(BITREVERSE)|
//          |(def(ISDN)&(def(ISDN_DRV_GIGASET)|def(ISDN_DRV_GIGASET_MODULE)))|
//          |def(CRC32_MODULE)|
//          |(((def(FB)&def(PCI)&def(HAS_IOMEM))|(def(HAS_IOMEM)&((def(FB)&def(PCI))|(def(PCI)&(def(FB)|def(FB_MODULE))))))&(def(FB_NVIDIA)|def(FB_NVIDIA_MODULE)))|
//          |(((def(X86)&def(PNP)&def(INPUT_MISC)&def(INPUT))|(def(X86)&def(PNP)&def(INPUT_MISC)&(def(INPUT)|def(INPUT_MODULE))))&(def(INPUT_WINBOND_CIR)|def(INPUT_WINBOND_CIR_MODULE)))|
//          |(((def(NETDEVICES)&def(ATM)&def(ATM_DRIVERS)&def(PCI)&def(VIRT_TO_BUS))|(def(ATM_DRIVERS)&((def(PCI)&def(VIRT_TO_BUS)&def(ATM)&def(NETDEVICES))|(def(NETDEVICES)&((def(PCI)&def(VIRT_TO_BUS)&def(ATM))|(def(PCI)&def(VIRT_TO_BUS)&(def(ATM)|def(ATM_MODULE))))))))&(def(ATM_AMBASSADOR)|def(ATM_AMBASSADOR_MODULE)))|
//          |(((def(RTC_CLASS)&def(I2C))|((def(I2C)|def(I2C_MODULE))&(def(RTC_CLASS_MODULE)|def(RTC_CLASS))))&(def(RTC_DRV_S35390A)|def(RTC_DRV_S35390A_MODULE)))|
//          |(def(BT_HCIUART_BCSP)&((def(BT)&def(BT_HCIUART)&def(NET))|(def(NET)&((def(BT)&def(BT_HCIUART))|((def(BT_HCIUART)|def(BT_HCIUART_MODULE))&(def(BT)|def(BT_MODULE)))))))|
//          |(((def(FDDI)&def(PCI)&def(NETDEVICES))|(def(NETDEVICES)&((def(FDDI)&def(PCI))|(def(PCI)&(def(FDDI)|def(FDDI_MODULE))))))&(def(SKFP)|def(SKFP_MODULE)))|
//          |(((def(HOTPLUG)&def(PCMCIA))|(def(HOTPLUG)&(def(PCMCIA)|def(PCMCIA_MODULE))))&(def(CARDMAN_4000)|def(CARDMAN_4000_MODULE)))|
//          |def(MODULES)|def(CRC32)|
//          |(((def(SND)&def(SND_PCI)&def(SOUND))|(((def(SND)&def(SND_PCI))|(def(SND_PCI)&(def(SND)|def(SND_MODULE))))&(def(SOUND)|def(SOUND_MODULE))))&(def(SND_ICE1712)|def(SND_ICE1712_MODULE)))|
//          |(def(NETDEVICES)&def(NET_ETHERNET)&def(HAS_IOMEM)&(def(ETHOC)|def(ETHOC_MODULE))&def(HAS_DMA))
//        """.stripMargin.replace("\n","")
//
//
//    def toBDD(expr: SATFeatureExpr): /*BDD*/FeatureExpr = {
//        expr match {
//            case And(clauses) =>
//                clauses.map(toBDD).foldLeft(BDDFeatureExprFactory.True)(_ and _)
//            case Or(clauses) =>
//                clauses.map(toBDD).foldLeft(BDDFeatureExprFactory.False)(_ or _)
//            case Not(e) =>
//                toBDD(e).not()
//            case DefinedExpr(n) => {
//                val bdd = BDDFeatureExprFactory.createDefinedExternal(n.feature)
//                //                    BDDFeatureExprHack.inc(bdd)
//                bdd
//            }
//            //                case DefinedExpr(n) => bddf.definedExternal(n.feature)
//
//
//        }
//    }
//
//    val sat=new FeatureExprParser(FeatureExprFactory.sat).parse(f)
////    val bdd=new FeatureExprParser(FeatureExprFactory.bdd).parse(f)
//    println(sat.collectDistinctFeatures.size)
//val bdd=toBDD(sat.asInstanceOf[SATFeatureExpr])
////    println(BDDFeatureExprHack.bdd(bdd.asInstanceOf[BDDFeatureExpr]))
//
//    def countClauses(c: BDD): Int = {
//        if (c.isZero)  1
//        else if (c.isOne) 0
//        else countClauses(c.high()) + countClauses(c.low())
//    }
//
//   println( countClauses(BDDFeatureExprHack.bdd(bdd.asInstanceOf[BDDFeatureExpr])))
////        .allsat().size())
////    println(bdd.bddAllSat.size)
//
//}