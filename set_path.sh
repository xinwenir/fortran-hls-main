#!/bin/bash
vi ~/.bashrc

# # Check if environment variables are already set in .bashrc
# if grep -q "FXX_LLVM_PATH" ~/.bashrc; then
#     echo -e "\033[33;1mWarning: The environment variables FXX_LLVM_PATH is already set in .bashrc. Skipping addition.\033[0m"
# else
#     echo -e "\033[32;1mAdding environment variable: FXX_LLVM_PATH to .bashrc...\033[0m"
#     # `FXX_LLVM_PATH`: path to the LLVM directory of Flang.
#     echo "export FXX_LLVM_PATH="/home/zxw/fxx/llvm-project/build/bin/"" >> ~/.bashrc
# fi
# # Check if environment variables are already set in .bashrc
# if grep -q "FXX_LLVM_PATH" ~/.bashrc; then
#     echo -e "\033[33;1mWarning: The environment variables FXX_LLVM_PATH is already set in .bashrc. Skipping addition.\033[0m"
# else
#     echo -e "\033[32;1mAdding environment variable: FXX_LLVM_PATH to .bashrc...\033[0m"
#     # `FXX_XILINX_LLVM_PATH`: path to the Xilinx LLVM directory.
#     echo "export FXX_XILINX_LLVM_PATH="/home/zxw/tools/Xilinx/Vitis/2021.2/llvm-clang/lnx64/llvm/bin"" >> ~/.bashrc
# fi
# # Check if environment variables are already set in .bashrc
# if grep -q "FXX_LLVM_PATH" ~/.bashrc; then
#     echo -e "\033[33;1mWarning: The environment variables FXX_LLVM_PATH is already set in .bashrc. Skipping addition.\033[0m"
# else
#     echo -e "\033[32;1mAdding environment variable: FXX_LLVM_PATH to .bashrc...\033[0m"
#     # `FXX_FLANG_PATH`: path to the Flang binary directory.
#     echo "export FXX_FLANG_PATH="/home/zxw/fxx/llvm-project/build/bin/"" >> ~/.bashrc
# fi
# # Check if environment variables are already set in .bashrc
# if grep -q "Vitis" ~/.bashrc; then
#     echo -e "\033[33;1mWarning: The environment variables FXX_LLVM_PATH is already set in .bashrc. Skipping addition.\033[0m"
# else
#     echo -e "\033[32;1mAdding environment variable: FXX_LLVM_PATH to .bashrc...\033[0m"
#     # `FXX_LLVM_PATH`: path to the LLVM directory of Flang.
#     echo "export PATH=/home/zxw/tools/Xilinx/Vitis/2021.2/bin:$PATH" >> ~/.bashrc
# fi

    #fortran-hls env
# `FXX_LLVM_PATH`: path to the LLVM directory of Flang.
echo "export FXX_LLVM_PATH="/home/zxw/fxx/llvm-project/build/bin/"" >> ~/.bashrc
# `FXX_XILINX_LLVM_PATH`: path to the Xilinx LLVM directory.
echo "export FXX_XILINX_LLVM_PATH="/home/zxw/tools/Xilinx/Vitis/2021.2/llvm-clang/lnx64/llvm/bin"" >> ~/.bashrc
# `FXX_FLANG_PATH`: path to the Flang binary directory.
echo "export FXX_FLANG_PATH="/home/zxw/fxx/llvm-project/build/bin/"" >> ~/.bashrc
#V++
echo "export PATH=/home/zxw/tools/Xilinx/Vitis/2021.2/bin:$PATH" >> ~/.bashrc

# 
source ~/.bashrc

echo $FXX_LLVM_PATH
echo $FXX_XILINX_LLVM_PATH
echo $FXX_FLANG_PATH

