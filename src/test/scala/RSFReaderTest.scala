package de.fosd.typechef.kconfig

import org.junit._
import java.io._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import FeatureExprFactory._
import scala._
import scala.sys.process.Process

class RSFReaderTest {

    def Item(s:String) = new Item(s,null)

    @Test def testDefault1 {
        val m = Item("m")
        m.setDefault("y", YTrue())

        assert(m.getDefaults()("y") equivalentTo True)
    }

    @Test def testDefault2 {
        val m = Item("m")
        m.setDefault("n", YTrue())

        assert(m.getDefaults().getOrElse("y",False) equivalentTo False)
    }

    val fa=createDefinedExternal("A")
    val fb=createDefinedExternal("B")
    val fc=createDefinedExternal("C")


    @Test def testDefault3 {
        val m = Item("m")
        m.setDefault("y", Name(Item("A")))

        assert(m.getDefaults()("y") equivalentTo fa)
    }

    @Test def testDefault4 {
        val m = Item("m")
        m.setDefault("y", Name(Item("A")))
        m.setDefault("n", Name(Item("B")))

        assert(m.getDefaults()("y") equivalentTo fa)
    }


    @Test def testDefault5 {
        val m = Item("m")
        m.setDefault("y", Name(Item("A")))
        m.setDefault("n",  YTrue())

        assert(m.getDefaults()("y") equivalentTo fa)
    }

    @Test def testDefault6 {
        val m = Item("m")
        m.setDefault("y", Name(Item("A")))
        m.setDefault("n", Name(Item("B")))
        m.setDefault("y", Name(Item("C")))

        assert(m.getDefaults()("y") equivalentTo (fa or (fc andNot fb)))
    }


    @Test def testDefault7 {
        val m = Item("m")
        m.setDefault("y", Name(Item("A")))
        m.setDefault("n",  YTrue())
        m.setDefault("y", Name(Item("B")))

        assert(m.getDefaults()("y") equivalentTo fa)
    }
}
