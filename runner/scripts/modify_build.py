import fileinput
import sys

basepath = sys.argv[1]

for line in fileinput.FileInput(basepath+"/project/BuildSettings.scala",inplace=1):
    if "libraryDependencies :=" in line:
        line=line.replace(line,line+"addCompilerPlugin(\"org.scalameta\" % \"semanticdb-scalac\" % scalafixSemanticdbVersion.value cross CrossVersion.full)")
    print line
