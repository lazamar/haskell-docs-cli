#! /bin/bash

# Command to run cram tests
# $TESTDIR is a variable made available by cram that contains the
# path to the directory where the test file lives.
# This means that all files must be in the same directory as this runner.

$TESTDIR/../../.stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build/hoogle-cli-exe/hoogle-cli-exe \
  --data-dir $TESTDIR/cram-data \
  --unlimited-cache \
  $@




