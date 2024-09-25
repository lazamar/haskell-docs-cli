#!/bin/bash

# This is a simple file for testing output of CLI commands.
# Each command's output is saved in a file in version control and
# tested against the live output of the command.
#
# Re-generating the files is straight-forward (see boolean flags in tests).
# New test files can be generated using the same method.

set -euo pipefail

# script's directory
DIR="$(dirname -- "${BASH_SOURCE[0]}")"

# temporary file to hold test outputs
TMP=$(mktemp)

# build beforehand to avoid build output during tests.
cabal build hdc
BINARY=$(cabal list-bin hdc)

# override hdc binary
function hdc() {
  set +u
  $BINARY \
    --cache-dir $DIR/cache \
    --cache unlimited \
    "${@}"
  set -u
}

# print coloured text
RED='\033[0;31m'
GREEN='\033[0;32m'
GRAY='\033[1;30m'
function color() {
  NC='\033[0m' # No Color
  printf "${1}${2}${NC}"
}

COUNT=0
FAILED=0

function run_test() {
  UPDATE="${1}" # update output
  COMMAND="${2}"
  OUTPUT="${3}"

  # Update output file
  if [ "$UPDATE" = true ] ; then
    eval "$COMMAND" &>$OUTPUT
  fi

  # Run the command
  eval "$COMMAND" &>$TMP
  diff $TMP $OUTPUT && R=true || R=false;

  # Report the result
  if [ $R = true ]
  then
    echo "-" $(color $GREEN "pass") $(color $GRAY "${COMMAND}")
  else
    echo "-" $(color $RED "failed") $(color $GRAY "${COMMAND}")
    FAILED=$(($FAILED + 1))
  fi
  COUNT=$(($COUNT + 1))
}

function cleanup() {
  rm $TMP
  echo "Done. $COUNT tests. $FAILED failures."
  if [ $FAILED -gt 0 ]; then
    exit 1
  fi
}

trap cleanup EXIT

# Change the boolean flag to True to update test output
run_test true "hdc --help" \
  "$DIR/outputs/test-0.output"
run_test false "hdc :help" \
  "$DIR/outputs/test-1.output"
run_test false "hdc :dd Bifunctor"  \
  "$DIR/outputs/test-2.output"
run_test false "hdc Map" \
  "$DIR/outputs/test-3.output"
run_test false "hdc pretty" \
  "$DIR/outputs/test-4.output"
run_test false "hdc :mi System.Console.Haskeline" \
  "$DIR/outputs/test-5.output"
run_test false "hdc :pd directory"  \
  "$DIR/outputs/test-6.output"
run_test false "echo getInputLine | hdc"  \
 "$DIR/outputs/test-7.output"




