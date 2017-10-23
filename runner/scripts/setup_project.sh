#!/bin/bash

if [ "$1" == "-h" ]; then
  echo "Usage: `./utils/$0 [project directory] [project name]` - Copy an sbt project into codebases/ and set it up for scalafix"
  echo "    Example: $ utils/setup_project.sh ../path/to/original/<project> <projectname>"
  exit 0
fi

BASENAME=$( basename $1 )
cp -r "$1" codebases/$BASENAME

echo "" >> codebases/$BASENAME/project/plugins.sbt
echo "//Scalafix plugin" >> codebases/$BASENAME/project/plugins.sbt
echo "addSbtPlugin(\"ch.epfl.scala\" % \"sbt-scalafix\" % \"0.5.3\")" >> codebases/$BASENAME/project/plugins.sbt
echo "addCompilerPlugin(\"org.scalameta\" % \"semanticdb-scalac\" % scalafixSemanticdbVersion.value cross CrossVersion.full)" >> codebases/$BASENAME/build.sbt