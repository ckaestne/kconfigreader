name := "kconfigreader"

version := "1.0"

organization := "de.fosd.typechef"

scalaVersion := "2.11.7"

parallelExecution := false

scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimise", "-feature")

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

libraryDependencies += "de.fosd.typechef" %% "featureexprlib" % "0.3.7"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"

libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5"

libraryDependencies += "de.fosd.typechef" % "javabdd_repackaged" % "1.0b2"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

//generate typechef.sh file with full classpath
TaskKey[File]("mkrun") <<= (baseDirectory, fullClasspath in Runtime, mainClass in Runtime) map {
    (base, cp, main) =>
        val template = """#!/bin/sh
java -ea -Xmx3G -Xms128m -Xss10m -classpath "%s" %s "$@"
                       """
        val contents = template.format(cp.files.absString, "")
        val out = base / "run.sh"
        IO.write(out, contents)
        out.setExecutable(true)
        out
}
