#!/bin/bash
. setup.sh

if [[ ! -d classic-flang-llvm-project ]]; then
    git clone -b release_16x https://github.com/flang-compiler/classic-flang-llvm-project.git
fi

cd classic-flang-llvm-project
mkdir -p build && cd build
cmake $CMAKE_OPTIONS -DCMAKE_C_COMPILER=/usr/bin/gcc -DCMAKE_CXX_COMPILER=/usr/bin/g++ \
-DCMAKE_BUILD_TYPE=Release \
-DLLVM_ENABLE_CLASSIC_FLANG=ON -DLLVM_ENABLE_PROJECTS="clang;openmp" ../llvm
make
sudo make install
