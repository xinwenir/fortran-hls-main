The following environment variables have to be defined before using `f++`:
* `FXX_LLVM_PATH`: path to the LLVM directory of Flang.
* `FXX_XILINX_LLVM_PATH`: path to the Xilinx LLVM directory.
* `FXX_FLANG_PATH`: path to the Flang binary directory.

## Exploration1:

### Install the [llvm-flang](https://github.com/flang-compiler/flang/wiki/Building-Flang):

- On a typical Ubuntu system, the build dependencies can be installed with the following command:
```
sudo apt-get install build-essential cmake ccache git libffi-dev libtinfo-dev ninja-build zlib1g-dev zstd
```
### Step-by-step instructions:
* 1. Create a build directory and define the CMake variables you will need. In the examples below, we will assume that you want to install in the install directory of wherever you will do the builds.  
```
cd /where/you/want/to/build/flang  
mkdir install
```

Here is a sample `setup.sh` that the other build scripts can use to define common variables. We specify a custom installation location, and indicate that we want to build for X86 with clang.  
```    
INSTALL_PREFIX=`pwd`/install   

# Targets to build should be one of: X86 PowerPC AArch64
CMAKE_OPTIONS="-DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX \
-DCMAKE_CXX_COMPILER=$INSTALL_PREFIX/bin/clang++ \
-DCMAKE_C_COMPILER=$INSTALL_PREFIX/bin/clang \
-DCMAKE_Fortran_COMPILER=$INSTALL_PREFIX/bin/flang 
-DCMAKE_Fortran_COMPILER_ID=Flang \
-DLLVM_TARGETS_TO_BUILD=X86"
```

* 2. Clone the llvm-project fork, build and install it (including Clang and OpenMP). Here is a `build-llvm-project.sh` script (using gcc and g++ to bootstrap the toolchain):  
```
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
```

* 3. Clone the flang repository, and build libpgmath and flang. Here's a sample `build-flang.sh` script (using clang to build). The script first builds libpgmath, and then builds the Classic Flang frontend and runtime libraries.  
Note that libpgmath on x86 requires a toolchain that understands AVX-512 instructions, such as gcc 7.2 or clang.
```
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
```
## Exploration2:

### Setting the following environment variables:

The following environment variables have to be defined before using `f++`:
* `FXX_LLVM_PATH`: path to the LLVM directory of Flang.
* `FXX_XILINX_LLVM_PATH`: path to the Xilinx LLVM directory.
* `FXX_FLANG_PATH`: path to the Flang binary directory.

Using the "set_path.sh" and adding:
```
#fortran-hls env
# `FXX_LLVM_PATH`: path to the LLVM directory of Flang.
export FXX_LLVM_PATH="/home/zxw/fxx/flang/install/bin"
# `FXX_XILINX_LLVM_PATH`: path to the Xilinx LLVM directory.
export FXX_XILINX_LLVM_PATH="/home/zxw/tools/Xilinx/Vitis/2021.2/llvm-clang/lnx64/llvm/bin"
# `FXX_FLANG_PATH`: path to the Flang binary directory.
export FXX_FLANG_PATH="/home/zxw/fxx/flang/install/bin"
#V++
export PATH=/home/zxw/tools/Xilinx/Vitis/2021.2/bin:$PATH
```
## switch different GCC/G++
```
sudo update-alternatives --remove gcc /usr/lib/gcc/x86_64-linux-gnu/9
sudo update-alternatives --remove gcc /usr/lib/gcc/x86_64-linux-gnu/10
sudo update-alternatives --remove gcc /usr/lib/gcc/x86_64-linux-gnu/11

sudo update-alternatives --install /usr/bin/gcc gcc /usr/lib/gcc/x86_64-linux-gnu/9 25
sudo update-alternatives --install /usr/bin/gcc gcc /usr/lib/gcc/x86_64-linux-gnu/10 20
sudo update-alternatives --install /usr/bin/gcc gcc /usr/lib/gcc/x86_64-linux-gnu/11 15

sudo update-alternatives --remove g++ /usr/include/c++/9
sudo update-alternatives --remove g++ /usr/include/c++/10
sudo update-alternatives --remove g++ /usr/include/c++/11

sudo update-alternatives --install /usr/bin/g++ g++ /usr/include/c++/9 25
sudo update-alternatives --install /usr/bin/g++ g++ /usr/include/c++/10 20
sudo update-alternatives --install /usr/bin/g++ g++ /usr/include/c++/11 15
```

## Install the flang-llvm:



## Install Vitis
The important step to avoid the error of `Install hangs at "Generating installed device list"`:
```
sudo apt-get install libtinfo5
cd Xilinx/install/packets/path
./xsetup
```

## Built `fortran_hls`
```
./set_virtualenv.sh
```

## Running:

activate venv:
```
 source venv/bin/activate
 fxx tests/unrll_test.f90 test1 0 unrll_top hw 1 oracle
```
close venv:
```
deactivate
```
