#!/bin/bash

if [ -z "$1" ]; then
  echo "Please provide test file name"
  exit 1
fi

TEST_FILE="../phase2TestCases/$1"
TEST_NAME=$(basename "$1" .c)

export LLVM_DIR=/opt/homebrew/opt/llvm/
cd build
cmake -DLT_LLVM_INSTALL_DIR=$LLVM_DIR ..
make

cd ../phase2TestCases

clang -c -emit-llvm -fno-discard-value-names -O0 "$TEST_FILE" -o "$TEST_NAME.bc"

opt -load-pass-plugin ../build/libLocalValueNumbering.dylib -passes=local-value-numbering "$TEST_NAME.bc" -o "$TEST_NAME.opt.bc"

llvm-dis "$TEST_NAME.opt.bc" -o "$TEST_NAME.ll"


echo "Test $TEST_NAME completed."

