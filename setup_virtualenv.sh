#!/bin/bash
python3 -m venv venv
source venv/bin/activate
python3 -m pip install -e .
mkdir -p llvm-passes-f18/build-v16
mkdir -p llvm-passes-f18/build-v7
cd llvm-passes-f18/build-v16
export LLVM_HOME=$FXX_LLVM_PATH/..
export CC=$LLVM_HOME/bin/clang
export CXX=$LLVM_HOME/bin/clang++
cmake -DV16=True ..
make

cd ../build-v7
export LLVM_HOME=$FXX_XILINX_LLVM_PATH/..
export CC=$LLVM_HOME/bin/clang
export CXX=$LLVM_HOME/bin/clang++
cmake -DV7=True ..
make
