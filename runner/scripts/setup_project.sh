#!/bin/bash

if [ "$1" == "-h" ]; then
  echo "Usage: `./utils/$0 [project directory] [project name]` - Copy an sbt project into codebases/ and set it up for scalafix"
  echo "    Example: $ utils/setup_project.sh ../path/to/original/<project> <projectname>"
  exit 0
fi

cp -r "$1" codebases/$2

echo "" >> codebases/$2/project/plugins.sbt
echo "//Scalafix plugin" >> codebases/$2/project/plugins.sbt
echo "addSbtPlugin(\"ch.epfl.scala\" % \"sbt-scalafix\" % \"0.5.2\")" >> codebases/$2/project/plugins.sbt
