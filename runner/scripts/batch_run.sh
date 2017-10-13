#!/bin/bash

if [ "$1" == "-h" ]; then
  echo "Usage: utils/batch_run.sh [scalafix command(util/run_scalafix.sh by default)] - Run the provided command through all the projects in codebases/"
  echo "    Example: $ utils/batch_run.sh utils/run_scalafix.sh"
  exit 0
fi

SCALAFIX_COMMAND=$1
if [ -z "$1" ]
  then
    echo "[Batch Runner] No scalafix command supplied, reset to default"
    SCALAFIX_COMMAND="utils/run_scalafix.sh"
fi

cd codebases
for project in $( find ./ -mindepth 1 -maxdepth 1 -type d )
do
    cd $project
    REPORTS="../../reports/$project"
    mkdir -p $REPORTS
    ../../$SCALAFIX_COMMAND #--report "$REPORTS"
    cd ..
done
cd ..