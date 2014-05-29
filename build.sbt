name := "TypeChef-kconfig"

version := "0.0.1"

organization := "de.fosd.typechef"

scalaVersion := "2.10.2"

libraryDependencies += "de.fosd.typechef" % "frontend_2.10" % "0.3.4"

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
