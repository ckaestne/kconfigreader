//package de.fosd.typechef.featureexpr.bdd
//
//import net.sf.javabdd.{BDDFactory, BDD}
//
//
///**
// * Created by energy on 6/10/14.
// */
//object BDDFeatureExprHack {
//    def resetBDD {
//
//        FExprBuilder.bddFactory.reset()
//        FExprBuilder.bddVarNum = 100
//        FExprBuilder.maxFeatureId = 0
//        FExprBuilder.featureIds.clear()
//
//        FExprBuilder.featureNames.clear()
//        FExprBuilder.featureBDDs.clear()
//        //        FExprBuilder.bddFactory = BDDFactory.init("JDD", FExprBuilder.bddValNum, FExprBuilder.bddCacheSize)
//        FExprBuilder.bddFactory.setIncreaseFactor(2) //200% increase each time
//        FExprBuilder.bddFactory.setMaxIncrease(0) //no upper limit on increase size
//        FExprBuilder.bddFactory.setVarNum(FExprBuilder.bddVarNum)
//
//    }
//
//
//
//
//    def free(f: BDDFeatureExpr) {
//        f.bdd.free()
//    }
//
//    def bdd(f:BDDFeatureExpr)=f.bdd
//}
