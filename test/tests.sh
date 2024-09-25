#!/bin/bash

# script's directory
DIR="$(dirname -- "${BASH_SOURCE[0]}")"

# temporary file to hold test outputs
TMP=$(mktemp)

# build beforehand to avoid build output during tests.
cabal build hdc
BINARY=$(cabal list-bin hdc)

# override hdc binary
function hdc() {
  $BINARY \
    --cache-dir $DIR/cache \
    --cache unlimited \
    "${@}"
}

# print coloured text
RED='\033[0;31m'
GREEN='\033[0;32m'
GRAY='\033[1;30m'
function color() {
  NC='\033[0m' # No Color
  printf "${1}${2}${NC}"
}

function run_test() {
  UPDATE="${1}" # update output
  COMMAND="${2}"
  OUTPUT="${3}"

  if [ "$UPDATE" = true ] ; then
    eval $COMMAND &>$OUTPUT
  fi

  eval $COMMAND &>$TMP
  diff $TMP $OUTPUT
  DIFF_EXIT_CODE=$?
  if [ $DIFF_EXIT_CODE -eq 0 ]
  then
    echo "-" $(color $GREEN "pass") $(color $GRAY "${COMMAND}")
  else
    echo "-" $(color $RED "failed") $(color $GRAY "${COMMAND}")
  fi
}

function cleanup() {
  echo "Done"
  rm $TMP
  exit 0
}

trap cleanup EXIT

# Change boolean to True to update test output
run_test false "hdc --help" \
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






