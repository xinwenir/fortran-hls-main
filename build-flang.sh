#!/bin/bash

. setup.sh

if [[ ! -d flang ]]; then
    git clone https://github.com/flang-compiler/flang.git
fi

(cd flang/runtime/libpgmath
mkdir -p build && cd build
cmake $CMAKE_OPTIONS ..
make
sudo make install)

cd flang
mkdir -p build && cd build
cmake $CMAKE_OPTIONS -DFLANG_LLVM_EXTENSIONS=ON ..
make
sudo make install
