#!/bin/bash

if [ $# -gt 0 ]
then
    if [ $1 = "init" ]
    then 
        echo "Clean the path and download the flang-llvm-project from Git:"
        mkdir -p /home/zxw/fxx
        cd /home/zxw/fxx
        git clone https://github.com/llvm/llvm-project.git

        cd llvm-project-V16
        echo "Create the Build-Path and Install-Path:"
        rm -rf build
        mkdir build
        rm -rf install
        mkdir install
    elif [ $1 = "crb" ]
    then
        echo "Clean & Rebuild:"
        cd /home/zxw/fxx/llvm-project-V16
        rm -rf build
        mkdir build
        rm -rf install
        mkdir install
    else #rb
        echo "Rebuild:"
        cd /home/zxw/fxx/llvm-project-V16
    fi
else 
    echo "Install the essential package:"
    sudo apt-get install build-essential cmake ccache git libffi-dev libtinfo-dev ninja-build zlib1g-dev zstd

    echo "init the path and download the flang-llvm-project from Git:"
    mkdir -p /home/zxw/fxx
    cd /home/zxw/fxx
    git clone https://github.com/llvm/llvm-project.git

    cd llvm-project-V16
    echo "Create the Build-Path and Install-Path:"
    rm -rf build
    mkdir build
    rm -rf install
    mkdir install
fi

ROOTDIR=`pwd`
INSTALLDIR=$ROOTDIR/install

cd build

cmake \
  -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=$INSTALLDIR \
  -DCMAKE_CXX_STANDARD=17 \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH" \
  -DFLANG_ENABLE_WERROR=ON \
  -DLLVM_ENABLE_ASSERTIONS=ON \
  -DLLVM_TARGETS_TO_BUILD=host \
  -DLLVM_LIT_ARGS=-v \
  -DLLVM_ENABLE_PROJECTS="clang;mlir;flang;openmp" \
  -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
  ../llvm

ninja 

ninja check-flang

ninja install
echo "latest" > $INSTALLDIR/bin/versionrc