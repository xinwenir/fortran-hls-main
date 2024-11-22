#!/bin/bash

mkdir -p /home/zxw/fxx
cd /home/zxw/fxx
git clone https://github.com/llvm/llvm-project.git
cd llvm-project

#sudo apt-get install build-essential cmake ccache git libffi-dev libtinfo-dev ninja-build zlib1g-dev zstd

rm -rf build
mkdir build
rm -rf install
mkdir install