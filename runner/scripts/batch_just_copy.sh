#!/bin/bash

CURDIR=$( pwd )


for project in $( find $1 -maxdepth 2 -name "build.sbt" )
do
	DIR=$( dirname $project )
	echo "Setting up project $DIR"

	BASENAME=$( basename $1 )
	cp -r "$DIR" codebases/$BASENAME

	#$CURDIR/utils/setup_project.sh $DIR
done
