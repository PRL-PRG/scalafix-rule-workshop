#!/bin/bash
# Instructions to run before commiting
echo 1>&2
echo "Pre-commit hook"

# Make sure that the code is formatted
./scripts/build/check-format.sh