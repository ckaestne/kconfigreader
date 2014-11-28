name := "kconfigreader"

version := "1.0"

organization := "de.fosd.typechef"

scalaVersion := "2.11.4"

parallelExecution := false

testOptions in Test += Tests.Argument("-verbosity", "1")

scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimise", "-feature")

//libraryDependencies += "de.fosd.typechef" %% "featureexprlib" % "0.3.7"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.2"

libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5"

libraryDependencies += "de.fosd.typechef" % "javabdd_repackaged" % "1.0b2"

libraryDependencies += "junit" % "junit" % "4.8.2" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.6" % "test"

testListeners <<= target.map(t => Seq(new eu.henkelmann.sbt.JUnitXmlTestsListener(t.getAbsolutePath)))

//generate typechef.sh file with full classpath
TaskKey[File]("mkrun") <<= (baseDirectory, fullClasspath in Runtime, mainClass in Runtime) map {
    (base, cp, main) =>
        val template = """#!/bin/sh
java -ea -Xmx1G -Xms128m -Xss10m -classpath "%s" %s "$@"
                       """
        val contents = template.format(cp.files.absString, "")
        val out = base / "run.sh"
        IO.write(out, contents)
        out.setExecutable(true)
        out
}
