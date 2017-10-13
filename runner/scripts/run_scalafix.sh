RULE="https://raw.githubusercontent.com/PRL-PRG/scalafix-rule-workshop/implicit-context/rules/src/main/scala/implicits/ImplicitContext.scala"
RULE_NAME=${RULE##*/}; RULE_NAME=${RULE_NAME%%.*}
COMMAND="time sbt -mem 2048 \"scalafix $RULE\""

if [ "$1" == "-h" ]; then
  echo "Usage: `./utils/$0 [--report \<report folder\>]` - Run scalafix in a given directory"
  exit 0
elif [ "$1" == "--report" ]; then
  echo "[Run Scalafix] Running Scalafix on $(  pwd )"
  echo "[Run Scalafix] Running rule: $RULE_NAME"
  echo "[Run Scalafix] Running command: $COMMAND"
  echo "[Run Scalafix] Reporting to $2/$RULE_NAME.txt"
  time sbt -mem 2048 "scalafix $RULE" > "$2/$RULE_NAME.txt"
else
  echo "[Run Scalafix] Running Scalafix on $(  pwd )"
  echo "[Run Scalafix] Running rule: $RULE_NAME"
  echo "[Run Scalafix] Running command: $COMMAND"
  echo "[Run Scalafix] Reporting to console"
  time sbt -mem 2048 "scalafix $RULE"
fi

