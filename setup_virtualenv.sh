#!/bin/bash
./makeclean.sh
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

#export PATH=/usr/include/c++/9:$PATH
#export LD_LIBRARY_PATH=/usr/lib/gcc/x86_64-linux-gnu/9:$LD_LIBRARY_PATH

export LLVM_HOME=$FXX_XILINX_LLVM_PATH/..
export LD_LIBRARY_PATH=$LLVM_HOME/lib:$LD_LIBRARY_PATH
export CC=$LLVM_HOME/bin/clang
export CXX=$LLVM_HOME/bin/clang++
cmake -DV7=True -DCMAKE_BUILD_TYPE=Release ..
make
