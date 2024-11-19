#!/bin/bash
vi ~/.bashrc

# 
source ~/.bashrc

echo $FXX_LLVM_PATH
echo $FXX_XILINX_LLVM_PATH
echo $FXX_FLANG_PATH

mkdir -p $FXX_LLVM_PATH
mkdir -p $FXX_XILINX_LLVM_PATH
mkdir -p $FXX_FLANG_PATH