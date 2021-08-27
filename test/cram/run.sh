#! /bin/bash

# Command to run cram tests
# $TESTDIR is a variable made available by cram that contains the
# path to the directory where the test file lives.
# This means that all files must be in the same directory as this runner.

cd $TESTDIR/../..
$(stack path --dist-dir)/build/hdc/hdc \
  --data-dir $TESTDIR/cram-data \
  --unlimited-cache \
  $@




