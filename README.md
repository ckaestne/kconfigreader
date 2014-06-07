kconfigreader
===========

tooling to read kconfig files and convert them into 
formulas for further reasoning (primarily for the
TypeChef infrastructure, but may be used elsewhere).

Build with `sbt`


To extract the raw data from kconfig files, this tool
relies on a patched version of undertaker's dumpconf tool
available here: https://github.com/ckaestne/undertaker
