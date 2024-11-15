# 打开.bashrc文件
vi ~/.bashrc

# `FXX_LLVM_PATH`: path to the LLVM directory of Flang.
export FXX_LLVM_PATH="/home/zxw/fxx/fxx_llvm"

# `FXX_XILINX_LLVM_PATH`: path to the Xilinx LLVM directory.
export FXX_XILINX_LLVM_PATH="/home/zxw/fxx/fxx_llvm"

# `FXX_FLANG_PATH`: path to the Flang binary directory.
export FXX_FLANG_PATH="/home/zxw/fxx/fxx_llvm"

# 保存并退出文件后，执行以下命令使设置生效
source ~/.bashrc