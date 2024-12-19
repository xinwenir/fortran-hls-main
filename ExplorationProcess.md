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
export FXX_LLVM_PATH="/home/zxw/fxx/llvm-project/build/bin/"
# `FXX_XILINX_LLVM_PATH`: path to the Xilinx LLVM directory.
export FXX_XILINX_LLVM_PATH="/home/zxw/tools/Xilinx/Vitis/2021.2/llvm-clang/lnx64/llvm/bin"
# `FXX_FLANG_PATH`: path to the Flang binary directory.
export FXX_FLANG_PATH="/home/zxw/fxx/llvm-project/build/bin/"
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
## Install `python3`
```
sudo apt install python3
sudo ln -s /usr/bin/python3 /usr/bin/python
sudo ln -s /usr/bin/python3-config /usr/bin/python-config
sudo apt install python3.10-venv
```
## Built `fortran_hls`
```
./set_virtualenv.sh
```

## Running:

activate venv:
```
source venv/bin/activate
fxx tests/pipe_test.f90 test1 1 pipe_test hw 1 0 -platform /home/zxw/project/xilinx_project/kv260_custom_platform/system_wrapper.xsa
```

close venv:
```
deactivate
```

## flang-new£º
OVERVIEW: flang LLVM compiler

USAGE: flang-new `[options]` file...

OPTIONS:  
 - -###                    Print (but do not run) the commands to run for this compilation  
 - -cpp                    Enable predefined and command line preprocessor macros  
 - -c                      Only run preprocess, compile, and assemble steps  
 - -D `<macro>`=`<value>`      Define `<macro>` to `<value>` (or 1 if `<value>` omitted)  
 - -emit-llvm              Use the LLVM representation for assembler and object files  
 - -E                      Only run the preprocessor  
 - -falternative-parameter-statement
                          Enable the old style PARAMETER statement  
 - -fapprox-func           Allow certain math function calls to be replaced with an approximately equivalent calculation  
 - -fbackslash             Specify that backslash in string introduces an escape character  
 - -fcolor-diagnostics     Enable colors in diagnostics  
 - -fconvert=`<value>`       Set endian conversion of data for unformatted files  
 - -fdefault-double-8      Set the default double precision kind to an 8 byte wide type  
 - -fdefault-integer-8     Set the default integer kind to an 8 byte wide type  
 - -fdefault-real-8        Set the default real kind to an 8 byte wide type  
 - -ffast-math             Allow aggressive, lossy floating-point optimizations  
 - -ffixed-form            Process source files in fixed form  
 - -ffixed-line-length=`<value>`
                          Use `<value>` as character line width in fixed mode  
 - -ffp-contract=`<value>`   Form fused FP ops (e.g. FMAs)  
 - -ffree-form             Process source files in free form  
 - -fimplicit-none         No implicit typing allowed unless overridden by IMPLICIT statements  
 - -finput-charset=`<value>` Specify the default character set for source files  
 - -fintrinsic-modules-path `<dir>`
                          Specify where to find the compiled intrinsic modules  
 - -flarge-sizes           Use INTEGER(KIND=8) for the result type in size-related intrinsics  
 - -flogical-abbreviations Enable logical abbreviations  
 - -fno-automatic          Implies the SAVE attribute for non-automatic local objects in subprograms unless RECURSIVE  
 - -fno-color-diagnostics  Disable colors in diagnostics  
 - -fno-integrated-as      Disable the integrated assembler  
 - -fno-signed-zeros       Allow optimizations that ignore the sign of floating point zeros  
 - -fopenacc               Enable OpenACC  
 - -fopenmp                Parse OpenMP pragmas and generate parallel code.  
 - -fpass-plugin=`<dsopath>` Load pass plugin from a dynamic shared object file (only with new pass manager).  
 - -freciprocal-math       Allow division operations to be reassociated  
 - -fsyntax-only           Run the preprocessor, parser and semantic analysis stages  
 - -fxor-operator          Enable .XOR. as a synonym of .NEQV.  
 - -help                   Display available options  
 - -I `<dir>`                Add directory to the end of the list of include search paths  
 - -mllvm `<value>`          Additional arguments to forward to LLVM's option processing  
 - -mmlir `<value>`          Additional arguments to forward to MLIR's option processing  
 - -module-dir `<dir>`       Put MODULE files in `<dir>`  
 - -nocpp                  Disable predefined and command line preprocessor macros  
 - -o `<file>`               Write output to `<file>`  
 - -pedantic               Warn on language extensions  
 - -print-effective-triple Print the effective target triple  
 - -print-target-triple    Print the normalized target triple  
 - -P                      Disable linemarker output in -E mode  
 - -save-temps=`<value>`     Save intermediate compilation results.  
 - -save-temps             Save intermediate compilation results  
 - -std=`<value>`            Language standard to compile for
 - -S                      Only run preprocess and compilation steps  
 - --target=`<value>`        Generate code for the given target  
 - -U `<macro>`              Undefine macro `<macro>`  
 - --version               Print version information  
 - -W`<warning>`             Enable the specified warning  
 - -Xflang `<arg>`           Pass `<arg>` to the flang compiler  
 - -x `<language>`           Treat subsequent input files as having type `<language>`  