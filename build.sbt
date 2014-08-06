name := "TypeChef-kconfig"

version := "0.0.2"

organization := "de.fosd.typechef"

scalaVersion := "2.10.4"

//libraryDependencies += "de.fosd.typechef" % "frontend_2.10" % "0.3.6"

libraryDependencies += "org.sat4j" % "org.sat4j.core" % "2.3.1"

libraryDependencies += "de.fosd.typechef" % "javabdd_repackaged" % "1.0b2"

libraryDependencies +="junit" % "junit" % "4.8.2" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.6" % "test"

//generate typechef.sh file with full classpath
TaskKey[File]("mkrun") <<= (baseDirectory, fullClasspath in Runtime, mainClass in Runtime) map { (base, cp, main) =>
  val template = """#!/bin/sh
java -ea -Xmx1G -Xms128m -Xss10m -classpath "%s" %s "$@"
"""
  val contents = template.format(cp.files.absString, "")
  val out = base / "run.sh"
  IO.write(out, contents)
  out.setExecutable(true)
  out
}
