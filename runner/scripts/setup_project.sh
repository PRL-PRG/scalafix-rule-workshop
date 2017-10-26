#!/bin/bash

COLOR='\033[0;36m'
NC='\033[0m' # No Color

if [ "$1" == "-h" ]; then
  echo "Usage: `./utils/$0 [project directory] [project name]` - Copy an sbt project into codebases/ and set it up for scalafix"
  echo "    Example: $ utils/setup_project.sh ../path/to/original/<project> <projectname>"
  exit 0
fi

CWD=$( pwd )

BASENAME=$( basename $1 )

echo -e "[${COLOR}$BASENAME Setup${NC}] Copy the project to codebases/"
cp -r "$1" codebases/$BASENAME

# Checkout latest tag
echo -e "[${COLOR}$BASENAME Setup${NC}] Checking out latest tag"
cd codebases/$BASENAME
echo $( cat $( find -L $CWD/ -name "checkout_tag.sh" ) | sh )
cd ../..

# Add scalafix plugin
echo -e "[${COLOR}$BASENAME Setup${NC}] Add scalafix plugin"
echo "" >> codebases/$BASENAME/project/plugins.sbt
echo "//Scalafix plugin" >> codebases/$BASENAME/project/plugins.sbt
echo "addSbtPlugin(\"ch.epfl.scala\" % \"sbt-scalafix\" % \"0.5.3\")" >> codebases/$BASENAME/project/plugins.sbt

# Allow scalafix to insert it's own compiler options
echo -e "[${COLOR}$BASENAME Setup${NC}] Change scalacOptions from := to ++="
find codebases/$BASENAME -name '*.sbt' -type f -print0 | xargs -0 sed -i 's/scalacOptions :=/scalacOptions ++=/g'

# Change scalaVersion from 
echo -e "[${COLOR}$BASENAME Setup${NC}] Change scalaVersion (2.11.X)"
find codebases/$BASENAME -name '*.sbt' -type f -print0 | xargs -0 sed -i 's/scalaVersion\s*:=\s*"2\.11\.\d*"/scalaVersion := "2.11.11"/g'
echo -e "[${COLOR}$BASENAME Setup${NC}] Change scalaVersion (2.12.X)"
find codebases/$BASENAME -name '*.sbt' -type f -print0 | xargs -0 sed -i 's/scalaVersion\s*:=\s*"2\.12\.\d*"/scalaVersion := "2.12.3"/g'

echo -e "[${COLOR}$BASENAME Setup${NC}] Done!"
