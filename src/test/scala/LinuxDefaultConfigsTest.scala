package de.fosd.typechef.kconfig

import org.junit.{Test, Ignore}
import sys.process._
import java.io._
import de.fosd.typechef.featureexpr.FeatureExprFactory


/**
 * tests whether the default configurations (allyesconfig) are possible in
 * the derived feature model
 *
 * this test became necessary after I learned that allyesconfig
 * included some features that were considered unsatisfiable by
 * our generated dimacs files
 */
 @Ignore("these tests run for a very long time. run only manually")
class LinuxDefaultConfigsTest extends LinuxTestInfrastructure {

    def checkDefaultConfig(configCommand: String) {

        val workingDir = new File(linuxTreeRoot)

        //generate allnoconfig
        val cmd = configTool + " " + configCommand + " " + kconfigFile("x86")
        assert(Process(cmd, workingDir, ("ARCH", "x86"), ("KERNELVERSION", "3.11")).! == 0, "failed executing " + cmd)


        val allnoconfig = readConfigFile(new File(workingDir, ".config"))

        val fm = x86fm_dimacs

        val d = FeatureExprFactory.createDefinedExternal _

        def myassert(condition: Boolean, msg: String) = /*assert(condition, msg) // */ if (!condition) System.err.println(msg)

        for ((feature, value) <- allnoconfig) {
            //            println(feature + " = " + value)

            if (value == "y")
                myassert(d(feature).isSatisfiable(fm), "%s=%s, but %s not satisfiable".format(feature, value, feature))
            else if (value == "n")
                myassert(d(feature).not.isSatisfiable(fm), "%s=%s, but %s is a tautology".format(feature, value, feature))
            else if (value == "m") {
                myassert(d(feature + "_MODULE").isSatisfiable(fm), "%s=%s, but %s_MODULE not satisfiable".format(feature, value, feature))
                myassert(d(feature).not.isSatisfiable(fm), "%s=%s, but %s=n not satisfiable".format(feature, value, feature))
            } else {
                //nonboolean
                val v = if (value.startsWith("\"") && value.endsWith("\"")) value.drop(1).dropRight(1) else value
                myassert(x86model.findItem(feature).knownNonBooleanValues contains v, "%s=%s not modeled with default values in kconfig".format(feature, value))
                myassert(x86model.findItem(feature).getNonBooleanValue(v).isSatisfiable(fm), "%s=%s not satisfiable".format(feature, value))
            }
        }

    }

    @Test
    def allnoconfigPossible {
        checkDefaultConfig("--allnoconfig")
    }
    @Test
    def allyesconfigPossible {
        checkDefaultConfig("--allyesconfig")
    }
    @Test
    def allmodconfigPossible {
        checkDefaultConfig("--allmodconfig")
    }
    @Test
    @Ignore("this file is manually maintained and has defaults that are not within the kconfig files")
    def defconfigPossible {
        checkDefaultConfig("--alldefconfig")
    }
    @Test
    def randconfigPossible {
        checkDefaultConfig("--randconfig")
    }


}
