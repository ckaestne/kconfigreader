package de.fosd.typechef.kconfig

import org.junit._
import java.io._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import FeatureExprFactory._
import scala._
import scala.sys.process.Process

class RSFReaderTest {

    @Test def testDefault1 {
        val m = Item("m")
        m.setDefault("y", ETrue())

        assert(m.getDefaultIsTrue().fexpr equivalentTo True)
    }

    @Test def testDefault2 {
        val m = Item("m")
        m.setDefault("n", ETrue())

        assert(m.getDefaultIsTrue().fexpr equivalentTo False)
    }

    val fa=createDefinedExternal("A")
    val fb=createDefinedExternal("B")
    val fc=createDefinedExternal("C")


    @Test def testDefault3 {
        val m = Item("m")
        m.setDefault("y", Name(Item("A")))

        assert(m.getDefaultIsTrue().fexpr equivalentTo fa)
    }

    @Test def testDefault4 {
        val m = Item("m")
        m.setDefault("y", Name(Item("A")))
        m.setDefault("n", Name(Item("B")))

        assert(m.getDefaultIsTrue().fexpr equivalentTo fa)
    }


    @Test def testDefault5 {
        val m = Item("m")
        m.setDefault("y", Name(Item("A")))
        m.setDefault("n",  ETrue())

        assert(m.getDefaultIsTrue().fexpr equivalentTo fa)
    }

    @Test def testDefault6 {
        val m = Item("m")
        m.setDefault("y", Name(Item("A")))
        m.setDefault("n", Name(Item("B")))
        m.setDefault("y", Name(Item("C")))

        assert(m.getDefaultIsTrue().fexpr equivalentTo (fa or (fc andNot fb)))
    }


    @Test def testDefault7 {
        val m = Item("m")
        m.setDefault("y", Name(Item("A")))
        m.setDefault("n",  ETrue())
        m.setDefault("y", Name(Item("B")))

        assert(m.getDefaultIsTrue().fexpr equivalentTo fa)
    }
}
