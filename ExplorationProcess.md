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
export FXX_LLVM_PATH="/home/zxw/fxx/llvm-project-V16/build/bin/"
# `FXX_XILINX_LLVM_PATH`: path to the Xilinx LLVM directory.
export FXX_XILINX_LLVM_PATH="/home/zxw/tools/Xilinx/Vitis/2021.2/llvm-clang/lnx64/llvm/bin"
# `FXX_FLANG_PATH`: path to the Flang binary directory.
export FXX_FLANG_PATH="/home/zxw/fxx/llvm-project-V16/build/bin/"
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

## Install the flang-llvm: ---------------->use this*****************************

chomd +777 flang-llvm-install.sh
./flang-llvm-install.sh

Notice:
  using your path and 
    mkdir -p /home/zxw/fxx
    cd /home/zxw/fxx


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

- activate venv:
```
source venv/bin/activate
fxx tests/pipe_test.f90 test1 1 pipe_test hw 1 0 -platform /home/zxw/project/xilinx_project/kv260_custom_platform/system_wrapper.xsa
```
- Test for music 

```
source ~/project/fortran-hls-main/venv/bin/activate
fxx test_music.f test_music 1 test_music hw 2 0 -platform /home/zxw/project/xilinx_project/kv260_custom_platform/system_wrapper.xsa -I src/ -temp_dir tmp 
fxx test_music.f test_music 1 test_music hw 1 0 -platform /home/zxw/project/xilinx_project/kv260_custom_platform/system_wrapper.xsa -I src/ -temp_dir tmp 
```

- close venv:
```
deactivate
```

- -parser.add_argument("filename", type=str, help="File to build")
- -parser.add_argument("output_file", type=str, help="Output name: <output_file>.xo, <output_file>.xclbin")
- -parser.add_argument("gen_llvm_ir", type=str, help="Generate LLVM IR: <0/1>") # TODO: this is just for debugging purposes. Remove for the artifact release
- -parser.add_argument("kernel_name", type=str, help="Name of the top function")
- -parser.add_argument("mode", type=str, help="Compilation mode. Supported: hw_emu, hw")
- -parser.add_argument("compile_stage", type=str, help="Compilation stage: 0 - skip v++; 1 - just compile; 2 - compile and link")
- -parser.add_argument("oracle", type=str, help="Generate oracle IR for unit tests")
- -parser.add_argument("-I", action='append', help="Include directory in compilation")
- -parser.add_argument("-k", action='append', help="List of kernel names. This has to be provided when the input is a .ll file")
- -parser.add_argument("-temp_dir", help="Name of the temporary directory")
- -parser.add_argument("-O0", action="store_const", const="O0", dest="O0", help="Optimization level 0")
- -parser.add_argument("-O1", action="store_const", const="O1", dest="O1", help="Optimization level 1")
- -parser.add_argument("-O2", action="store_const", const="O2", dest="O2", help="Optimization level 2")
- -parser.add_argument("-O3", action="store_const", const="O3", dest="O3", help="Optimization level 3")
- -parser.add_argument("-xilinx_generated", action="store_true")
- -parser.add_argument("-platform")


## flang-new:
OVERVIEW: flang LLVM compiler

USAGE: flang-new `[options]` file...

OPTIONS:  
 - -###                    Print (but do not run) the commands to run for this compilation  
 - -cpp                    Enable predefined and command line preprocessor macros  
 - -c                      Only run preprocess, compile, and assemble steps  
 - -D `<macro>`=`<value>`      Define `<macro>` to `<value>` (or 1 if `<value>` omitted)  
 - -emit-llvm              Use the LLVM representation for assembler and object files  
 - -E                      Only run the preprocessor  
 - -falternative-parameter-statement Enable the old style PARAMETER statement  
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

 ## llvm/opt:

 OVERVIEW: llvm .bc -> .bc modular optimizer and analysis printer

USAGE: opt [options] <input bitcode file>

OPTIONS:

Color Options:

  --color                                                               - Use colors in output (default=autodetect)

General options:

  --O0                                                                  - Optimization level 0. Similar to clang -O0. Use -passes='default<O0>' for the new PM
  --O1                                                                  - Optimization level 1. Similar to clang -O1. Use -passes='default<O1>' for the new PM
  --O2                                                                  - Optimization level 2. Similar to clang -O2. Use -passes='default<O2>' for the new PM
  --O3                                                                  - Optimization level 3. Similar to clang -O3. Use -passes='default<O3>' for the new PM
  --Os                                                                  - Like -O2 but size-conscious. Similar to clang -Os. Use -passes='default<Os>' for the new PM
  --Oz                                                                  - Like -O2 but optimize for code size above all else. Similar to clang -Oz. Use -passes='default<Oz>' for the new PM
  -S                                                                    - Write output as LLVM assembly
  --abort-on-max-devirt-iterations-reached                              - Abort when the max iterations for devirtualization CGSCC repeat pass is reached
  --addrsig                                                             - Emit an address-significance table
  --align-loops=<uint>                                                  - Default alignment for loops
  --allow-ginsert-as-artifact                                           - Allow G_INSERT to be considered an artifact. Hack around AMDGPU test infinite loops.
  --asm-show-inst                                                       - Emit internal instruction representation to assembly file
  --atomic-counter-update-promoted                                      - Do counter update using atomic fetch add  for promoted counters only
  Optimizations available (use '-passes=' for the new pass manager)
      --aa                                                                 - Function Alias Analysis Results
      --aa-eval                                                            - Exhaustive Alias Analysis Precision Evaluator
      --adce                                                               - Aggressive Dead Code Elimination
      --add-discriminators                                                 - Add DWARF path discriminators
      --alignment-from-assumptions                                         - Alignment from assumptions
      --always-inline                                                      - Inliner for always_inline functions
      --annotation2metadata                                                - Annotation2Metadata
      --assume-builder                                                     - Assume Builder
      --assume-simplify                                                    - Assume Simplify
      --assumption-cache-tracker                                           - Assumption Cache Tracker
      --atomic-expand                                                      - Expand Atomic instructions
      --attributor                                                         - Deduce and propagate attributes
      --attributor-cgscc                                                   - Deduce and propagate attributes (CGSCC pass)
      --barrier                                                            - A No-Op Barrier Pass
      --basic-aa                                                           - Basic Alias Analysis (stateless AA impl)
      --basiccg                                                            - CallGraph Construction
      --bbsections-profile-reader                                          - Reads and parses a basic block sections profile.
      --bdce                                                               - Bit-Tracking Dead Code Elimination
      --block-freq                                                         - Block Frequency Analysis
      --branch-prob                                                        - Branch Probability Analysis
      --break-crit-edges                                                   - Break critical edges in CFG
      --called-value-propagation                                           - Called Value Propagation
      --callsite-splitting                                                 - Call-site splitting
      --canon-freeze                                                       - Canonicalize Freeze Instructions in Loops
      --check-debugify                                                     - Check debug info from -debugify
      --check-debugify-function                                            - Check debug info from -debugify-function
      --codegenprepare                                                     - Optimize for code generation
      --consthoist                                                         - Constant Hoisting
      --constmerge                                                         - Merge Duplicate Global Constants
      --correlated-propagation                                             - Value Propagation
      --cost-model                                                         - Cost Model Analysis
      --cross-dso-cfi                                                      - Cross-DSO CFI
      --cseinfo                                                            - Analysis containing CSE Info
      --cycles                                                             - Cycle Info Analysis
      --da                                                                 - Dependence Analysis
      --dce                                                                - Dead Code Elimination
      --deadargelim                                                        - Dead Argument Elimination
      --deadarghaX0r                                                       - Dead Argument Hacking (BUGPOINT USE ONLY; DO NOT USE)
      --debugify                                                           - Attach debug info to everything
      --debugify-function                                                  - Attach debug info to a function
      --delinearize                                                        - Delinearization
      --demanded-bits                                                      - Demanded bits analysis
      --dfa-jump-threading                                                 - DFA Jump Threading
      --div-rem-pairs                                                      - Hoist/decompose integer division and remainder
      --divergence                                                         - Legacy Divergence Analysis
      --domfrontier                                                        - Dominance Frontier Construction
      --domtree                                                            - Dominator Tree Construction
      --dot-callgraph                                                      - Print call graph to 'dot' file
      --dot-cfg                                                            - Print CFG of function to 'dot' file
      --dot-cfg-only                                                       - Print CFG of function to 'dot' file (with no function bodies)
      --dot-dom                                                            - Print dominance tree of function to 'dot' file
      --dot-dom-only                                                       - Print dominance tree of function to 'dot' file (with no function bodies)
      --dot-postdom                                                        - Print postdominance tree of function to 'dot' file
      --dot-postdom-only                                                   - Print postdominance tree of function to 'dot' file (with no function bodies)
      --dot-regions                                                        - Print regions of function to 'dot' file
      --dot-regions-only                                                   - Print regions of function to 'dot' file (with no function bodies)
      --dse                                                                - Dead Store Elimination
      --dwarfehprepare                                                     - Prepare DWARF exceptions
      --early-cse                                                          - Early CSE
      --early-cse-memssa                                                   - Early CSE w/ MemorySSA
      --edge-bundles                                                       - Bundle Machine CFG Edges
      --elim-avail-extern                                                  - Eliminate Available Externally Globals
      --expand-large-div-rem                                               - Expand large div/rem
      --expand-large-fp-convert                                            - Expand large fp convert
      --expand-reductions                                                  - Expand reduction intrinsics
      --expandmemcmp                                                       - Expand memcmp() to load/stores
      --expandvp                                                           - Expand vector predication intrinsics
      --external-aa                                                        - External Alias Analysis
      --fastpretileconfig                                                  - Fast Tile Register Preconfigure
      --fasttileconfig                                                     - Fast Tile Register Configure
      --fix-irreducible                                                    - Convert irreducible control-flow into natural loops
      --flattencfg                                                         - Flatten the CFG
      --float2int                                                          - Float to int
      --forceattrs                                                         - Force set function attributes
      --function-attrs                                                     - Deduce function attributes
      --gisel-known-bits                                                   - Analysis for ComputingKnownBits
      --global-merge                                                       - Merge global variables
      --globaldce                                                          - Dead Global Elimination
      --globalopt                                                          - Global Variable Optimizer
      --globals-aa                                                         - Globals Alias Analysis
      --globalsplit                                                        - Global splitter
      --guard-widening                                                     - Widen guards
      --gvn                                                                - Global Value Numbering
      --gvn-hoist                                                          - Early GVN Hoisting of Expressions
      --gvn-sink                                                           - Early GVN sinking of Expressions
      --hardware-loops                                                     - Hardware Loop Insertion
      --hotcoldsplit                                                       - Hot Cold Splitting
      --indirectbr-expand                                                  - Expand indirectbr instructions
      --indvars                                                            - Induction Variable Simplification
      --infer-address-spaces                                               - Infer address spaces
      --inferattrs                                                         - Infer set function attributes
      --inject-tli-mappings                                                - Inject TLI Mappings
      --inline                                                             - Function Integration/Inlining
      --instcombine                                                        - Combine redundant instructions
      --instcount                                                          - Counts the various types of Instructions
      --instnamer                                                          - Assign names to anonymous instructions
      --instruction-select                                                 - Select target instructions out of generic instructions
      --instsimplify                                                       - Remove redundant instructions
      --interleaved-access                                                 - Lower interleaved memory accesses to target specific intrinsics
      --interleaved-load-combine                                           - Combine interleaved loads into wide loads and shufflevector instructions
      --internalize                                                        - Internalize Global Symbols
      --intervals                                                          - Interval Partition Construction
      --ipsccp                                                             - Interprocedural Sparse Conditional Constant Propagation
      --ir-similarity-identifier                                           - ir-similarity-identifier
      --irce                                                               - Inductive range check elimination
      --iroutliner                                                         - IR Outliner
      --irtranslator                                                       - IRTranslator LLVM IR -> MI
      --iv-users                                                           - Induction Variable Users
      --jmc-instrument                                                     - Instrument function entry with call to __CheckForDebuggerJustMyCode
      --jump-threading                                                     - Jump Threading
      --lazy-block-freq                                                    - Lazy Block Frequency Analysis
      --lazy-branch-prob                                                   - Lazy Branch Probability Analysis
      --lazy-value-info                                                    - Lazy Value Information Analysis
      --lcssa                                                              - Loop-Closed SSA Form Pass
      --lcssa-verification                                                 - LCSSA Verifier
      --legalizer                                                          - Legalize the Machine IR a function's Machine IR
      --libcalls-shrinkwrap                                                - Conditionally eliminate dead library calls
      --licm                                                               - Loop Invariant Code Motion
      --lint                                                               - Statically lint-checks LLVM IR
      --load-store-vectorizer                                              - Vectorize load and store instructions
      --loadstore-opt                                                      - Generic memory optimizations
      --localizer                                                          - Move/duplicate certain instructions close to their use
      --loop-accesses                                                      - Loop Access Analysis
      --loop-data-prefetch                                                 - Loop Data Prefetch
      --loop-deletion                                                      - Delete dead loops
      --loop-distribute                                                    - Loop Distribution
      --loop-extract                                                       - Extract loops into new functions
      --loop-extract-single                                                - Extract at most one loop into a new function
      --loop-flatten                                                       - Flattens loops
      --loop-fusion                                                        - Loop Fusion
      --loop-guard-widening                                                - Widen guards (within a single loop, as a loop pass)
      --loop-idiom                                                         - Recognize loop idioms
      --loop-instsimplify                                                  - Simplify instructions in loops
      --loop-interchange                                                   - Interchanges loops for cache reuse
      --loop-load-elim                                                     - Loop Load Elimination
      --loop-predication                                                   - Loop predication
      --loop-reduce                                                        - Loop Strength Reduction
      --loop-reroll                                                        - Reroll loops
      --loop-rotate                                                        - Rotate Loops
      --loop-simplify                                                      - Canonicalize natural loops
      --loop-simplifycfg                                                   - Simplify loop CFG
      --loop-sink                                                          - Loop Sink
      --loop-unroll                                                        - Unroll loops
      --loop-unroll-and-jam                                                - Unroll and Jam loops
      --loop-vectorize                                                     - Loop Vectorization
      --loop-versioning                                                    - Loop Versioning
      --loop-versioning-licm                                               - Loop Versioning For LICM
      --loops                                                              - Natural Loop Information
      --lower-amx-intrinsics                                               - Lower AMX intrinsics
      --lower-amx-type                                                     - Lower AMX type for load/store
      --lower-constant-intrinsics                                          - Lower constant intrinsics
      --lower-expect                                                       - Lower 'expect' Intrinsics
      --lower-global-dtors                                                 - Lower @llvm.global_dtors via `__cxa_atexit`
      --lower-guard-intrinsic                                              - Lower the guard intrinsic to normal control flow
      --lower-matrix-intrinsics                                            - Lower the matrix intrinsics
      --lower-matrix-intrinsics-minimal                                    - Lower the matrix intrinsics (minimal)
      --lower-widenable-condition                                          - Lower the widenable condition to default true value
      --loweratomic                                                        - Lower atomic intrinsics to non-atomic form
      --lowerinvoke                                                        - Lower invoke and unwind, for unwindless code generators
      --lowerswitch                                                        - Lower SwitchInst's to branches
      --lowertilecopy                                                      - Tile Copy Lowering
      --machine-block-freq                                                 - Machine Block Frequency Analysis
      --machine-branch-prob                                                - Machine Branch Probability Analysis
      --machine-domfrontier                                                - Machine Dominance Frontier Construction
      --machine-loops                                                      - Machine Natural Loop Construction
      --machinedomtree                                                     - MachineDominator Tree Construction
      --make-guards-explicit                                               - Lower the guard intrinsic to explicit control flow form
      --mem2reg                                                            - Promote Memory to Register
      --memcpyopt                                                          - MemCpy Optimization
      --memdep                                                             - Memory Dependence Analysis
      --memoryssa                                                          - Memory SSA
      --mergefunc                                                          - Merge Functions
      --mergeicmps                                                         - Merge contiguous icmps into a memcmp
      --mergereturn                                                        - Unify function exit nodes
      --metarenamer                                                        - Assign new names to everything
      --mldst-motion                                                       - MergedLoadStoreMotion
      --module-debuginfo                                                   - Decodes module-level debug info
      --module-summary-analysis                                            - Module Summary Analysis
      --module-summary-info                                                - Module summary info
      --nary-reassociate                                                   - Nary reassociation
      --newgvn                                                             - Global Value Numbering
      --opt-remark-emitter                                                 - Optimization Remark Emitter
      --partial-inliner                                                    - Partial Inliner
      --partially-inline-libcalls                                          - Partially inline calls to library functions
      --phi-values                                                         - Phi Values Analysis
      --place-backedge-safepoints-impl                                     - Place Backedge Safepoints
      --place-safepoints                                                   - Place Safepoints
      --postdomtree                                                        - Post-Dominator Tree Construction
      --pre-amx-config                                                     - Pre AMX Tile Config
      --pre-isel-intrinsic-lowering                                        - Pre-ISel Intrinsic Lowering
      --print-callgraph                                                    - Print a call graph
      --print-externalfnconstants                                          - Print external fn callsites passed constants
      --print-function                                                     - Print function to stderr
      --print-lazy-value-info                                              - Lazy Value Info Printer Pass
      --print-memdeps                                                      - Print MemDeps of function
      --print-memderefs                                                    - Memory Dereferenciblity of pointers in function
      --print-memoryssa                                                    - Memory SSA Printer
      --print-module                                                       - Print module to stderr
      --print-must-be-executed-contexts                                    - print the must-be-executed-context for all instructions
      --print-mustexecute                                                  - Instructions which execute on loop entry
      --print-predicateinfo                                                - PredicateInfo Printer
      --profile-summary-info                                               - Profile summary info
      --pseudo-probe-inserter                                              - Insert pseudo probe annotations for value profiling
      --reaching-deps-analysis                                             - ReachingDefAnalysis
      --reassociate                                                        - Reassociate expressions
      --redundant-dbg-inst-elim                                            - Redundant Dbg Instruction Elimination
      --reg2mem                                                            - Demote all values to stack slots
      --regbankselect                                                      - Assign register bank of generic virtual registers
      --regions                                                            - Detect single entry single exit regions
      --replace-with-veclib                                                - Replace intrinsics with calls to vector library
      --rewrite-statepoints-for-gc                                         - Make relocations explicit at statepoints
      --rewrite-symbols                                                    - Rewrite Symbols
      --rpo-function-attrs                                                 - Deduce function attributes in RPO
      --safe-stack                                                         - Safe Stack instrumentation pass
      --scalar-evolution                                                   - Scalar Evolution Analysis
      --scalarize-masked-mem-intrin                                        - Scalarize unsupported masked memory intrinsics
      --scalarizer                                                         - Scalarize vector operations
      --sccp                                                               - Sparse Conditional Constant Propagation
      --scev-aa                                                            - ScalarEvolution-based Alias Analysis
      --scoped-noalias-aa                                                  - Scoped NoAlias Alias Analysis
      --select-optimize                                                    - Optimize selects
      --separate-const-offset-from-gep                                     - Split GEPs to a variadic base and a constant offset for better CSE
      --simple-loop-unswitch                                               - Simple unswitch loops
      --simplifycfg                                                        - Simplify the CFG
      --sink                                                               - Code sinking
      --sjljehprepare                                                      - Prepare SjLj exceptions
      --slp-vectorizer                                                     - SLP Vectorizer
      --slsr                                                               - Straight line strength reduction
      --speculative-execution                                              - Speculatively execute instructions
      --sroa                                                               - Scalar Replacement Of Aggregates
      --stack-protector                                                    - Insert stack protectors
      --stack-safety                                                       - Stack Safety Analysis
      --stack-safety-local                                                 - Stack Safety Local Analysis
      --strip                                                              - Strip all symbols from a module
      --strip-dead-debug-info                                              - Strip debug info for unused symbols
      --strip-dead-prototypes                                              - Strip Unused Function Prototypes
      --strip-debug-declare                                                - Strip all llvm.dbg.declare intrinsics
      --strip-gc-relocates                                                 - Strip gc.relocates inserted through RewriteStatepointsForGC
      --strip-nondebug                                                     - Strip all symbols, except dbg symbols, from a module
      --strip-nonlinetable-debuginfo                                       - Strip all debug info except linetables
      --structurizecfg                                                     - Structurize the CFG
      --tailcallelim                                                       - Tail Call Elimination
      --targetlibinfo                                                      - Target Library Information
      --targetpassconfig                                                   - Target Pass Configuration
      --tbaa                                                               - Type-Based Alias Analysis
      --tileconfig                                                         - Tile Register Configure
      --tilepreconfig                                                      - Tile Register Pre-configure
      --tlshoist                                                           - TLS Variable Hoist
      --transform-warning                                                  - Warn about non-applied transformations
      --tti                                                                - Target Transform Information
      --unify-loop-exits                                                   - Fixup each natural loop to have a single exit block
      --unreachableblockelim                                               - Remove unreachable blocks from the CFG
      --vector-combine                                                     - Optimize scalar/vector ops
      --verify                                                             - Module Verifier
      --verify-safepoint-ir                                                - Safepoint IR Verifier
      --view-callgraph                                                     - View call graph
      --view-cfg                                                           - View CFG of function
      --view-cfg-only                                                      - View CFG of function (with no function bodies)
      --view-dom                                                           - View dominance tree of function
      --view-dom-only                                                      - View dominance tree of function (with no function bodies)
      --view-postdom                                                       - View postdominance tree of function
      --view-postdom-only                                                  - View postdominance tree of function (with no function bodies)
      --view-regions                                                       - View regions of function
      --view-regions-only                                                  - View regions of function (with no function bodies)
      --virtregmap                                                         - Virtual Register Map
      --wasmehprepare                                                      - Prepare WebAssembly exceptions
      --winehprepare                                                       - Prepare Windows exceptions
      --write-bitcode                                                      - Write Bitcode
      --x86-avoid-SFB                                                      - Machine code sinking
      --x86-avoid-trailing-call                                            - X86 avoid trailing call pass
      --x86-cf-opt                                                         - X86 Call Frame Optimization
      --x86-cmov-conversion                                                - X86 cmov Conversion
      --x86-codegen                                                        - X86 FP Stackifier
      --x86-domain-reassignment                                            - X86 Domain Reassignment Pass
      --x86-evex-to-vex-compress                                           - Compressing EVEX instrs to VEX encoding when possible
      --x86-execution-domain-fix                                           - X86 Execution Domain Fix
      --x86-fixup-LEAs                                                     - X86 LEA Fixup
      --x86-fixup-bw-insts                                                 - X86 Byte/Word Instruction Fixup
      --x86-fixup-setcc                                                    - x86-fixup-setcc
      --x86-flags-copy-lowering                                            - X86 EFLAGS copy lowering
      --x86-isel                                                           - X86 DAG->DAG Instruction Selection
      --x86-kcfi                                                           - Insert KCFI indirect call checks
      --x86-lvi-load                                                       - X86 LVI load hardening
      --x86-lvi-ret                                                        - X86 LVI ret hardener
      --x86-optimize-LEAs                                                  - X86 optimize LEA pass
      --x86-partial-reduction                                              - X86 Partial Reduction
      --x86-pseudo                                                         - X86 pseudo instruction expansion pass
      --x86-return-thunks                                                  - X86 Return Thunks
      --x86-seses                                                          - X86 Speculative Execution Side Effect Suppression
      --x86-slh                                                            - X86 speculative load hardener
      --x86-winehstate                                                     - Insert stores for EH state numbers
  --atomic-first-counter                                                - Use atomic fetch add for first counter in a function (usually the entry counter)
  --basic-block-sections=<all | <function list (file)> | labels | none> - Emit basic blocks into separate sections
  --bounds-checking-single-trap                                         - Use one trap block per function
  --cfg-hide-cold-paths=<number>                                        - Hide blocks with relative frequency below the given value
  --cfg-hide-deoptimize-paths                                           - 
  --cfg-hide-unreachable-paths                                          - 
  --code-model=<value>                                                  - Choose code model
    =tiny                                                               -   Tiny code model
    =small                                                              -   Small code model
    =kernel                                                             -   Kernel code model
    =medium                                                             -   Medium code model
    =large                                                              -   Large code model
  --codegen-opt-level=<uint>                                            - Override optimization level for codegen hooks, legacy PM only
  --cost-kind=<value>                                                   - Target cost kind
    =throughput                                                         -   Reciprocal throughput
    =latency                                                            -   Instruction latency
    =code-size                                                          -   Code size
    =size-latency                                                       -   Code size and latency
  --data-layout=<layout-string>                                         - data layout string to use
  --data-sections                                                       - Emit data into separate sections
  --debug-entry-values                                                  - Enable debug info for the debug entry values.
  --debug-info-correlate                                                - Use debug info to correlate profiles.
  --debugger-tune=<value>                                               - Tune debug info for a particular debugger
    =gdb                                                                -   gdb
    =lldb                                                               -   lldb
    =dbx                                                                -   dbx
    =sce                                                                -   SCE targets (e.g. PS4)
  --debugify-each                                                       - Start each pass with debugify and end it with check-debugify
  --debugify-export=<filename>                                          - Export per-pass debugify statistics to this file
  --debugify-func-limit=<ulong>                                         - Set max number of processed functions per pass.
  --debugify-level=<value>                                              - Kind of debug info to add
    =locations                                                          -   Locations only
    =location+variables                                                 -   Locations and Variables
  --debugify-quiet                                                      - Suppress verbose debugify output
  --denormal-fp-math=<value>                                            - Select which denormal numbers the code is permitted to require
    =ieee                                                               -   IEEE 754 denormal numbers
    =preserve-sign                                                      -   the sign of a  flushed-to-zero number is preserved in the sign of 0
    =positive-zero                                                      -   denormals are flushed to positive zero
  --denormal-fp-math-f32=<value>                                        - Select which denormal numbers the code is permitted to require for float
    =ieee                                                               -   IEEE 754 denormal numbers
    =preserve-sign                                                      -   the sign of a  flushed-to-zero number is preserved in the sign of 0
    =positive-zero                                                      -   denormals are flushed to positive zero
  --disable-builtin=<string>                                            - Disable specific target library builtin function
  --disable-debug-info-type-map                                         - Don't use a uniquing type map for debug info
  --disable-i2p-p2i-opt                                                 - Disables inttoptr/ptrtoint roundtrip optimization
  --disable-loop-unrolling                                              - Disable loop unrolling in all relevant passes
  --disable-simplify-libcalls                                           - Disable simplify-libcalls
  --disable-tail-calls                                                  - Never emit tail calls
  --do-counter-promotion                                                - Do counter register promotion
  --dot-cfg-mssa=<file name for generated dot file>                     - file name for generated dot file
  --dwarf-version=<int>                                                 - Dwarf version
  --dwarf64                                                             - Generate debugging info in the 64-bit DWARF format
  --emit-call-site-info                                                 - Emit call site debug information, if debug information is enabled.
  --emit-dwarf-unwind=<value>                                           - Whether to emit DWARF EH frame entries.
    =always                                                             -   Always emit EH frame entries
    =no-compact-unwind                                                  -   Only emit EH frame entries when compact unwind is not available
    =default                                                            -   Use target platform default
  --emulated-tls                                                        - Use emulated TLS model
  --enable-approx-func-fp-math                                          - Enable FP math optimizations that assume approx func
  --enable-cse-in-irtranslator                                          - Should enable CSE in irtranslator
  --enable-cse-in-legalizer                                             - Should enable CSE in Legalizer
  --enable-debugify                                                     - Start the pipeline with debugify and end it with check-debugify
  --enable-gvn-hoist                                                    - Enable the GVN hoisting pass (default = off)
  --enable-gvn-memdep                                                   - 
  --enable-gvn-sink                                                     - Enable the GVN sinking pass (default = off)
  --enable-jmc-instrument                                               - Instrument functions with a call to __CheckForDebuggerJustMyCode
  --enable-load-in-loop-pre                                             - 
  --enable-load-pre                                                     - 
  --enable-loop-simplifycfg-term-folding                                - 
  --enable-name-compression                                             - Enable name/filename string compression
  --enable-new-pm                                                       - Enable the new pass manager, translating 'opt -foo' to 'opt -passes=foo'. This is strictly for the new PM migration, use '-passes=' when possible.
  --enable-no-infs-fp-math                                              - Enable FP math optimizations that assume no +-Infs
  --enable-no-nans-fp-math                                              - Enable FP math optimizations that assume no NaNs
  --enable-no-signed-zeros-fp-math                                      - Enable FP math optimizations that assume the sign of 0 is insignificant
  --enable-no-trapping-fp-math                                          - Enable setting the FP exceptions build attribute not to use exceptions
  --enable-split-backedge-in-load-pre                                   - 
  --enable-unsafe-fp-math                                               - Enable optimizations that may decrease FP precision
  --exception-model=<value>                                             - exception model
    =default                                                            -   default exception handling model
    =dwarf                                                              -   DWARF-like CFI based exception handling
    =sjlj                                                               -   SjLj exception handling
    =arm                                                                -   ARM EHABI exceptions
    =wineh                                                              -   Windows exception model
    =wasm                                                               -   WebAssembly exception handling
  --experimental-debug-variable-locations                               - Use experimental new value-tracking variable locations
  -f                                                                    - Enable binary output on terminals
  --fatal-warnings                                                      - Treat warnings as errors
  --filetype=<value>                                                    - Choose a file type (not all types are supported by all targets):
    =asm                                                                -   Emit an assembly ('.s') file
    =obj                                                                -   Emit a native object ('.o') file
    =null                                                               -   Emit nothing, for performance testing
  --float-abi=<value>                                                   - Choose float ABI type
    =default                                                            -   Target default float ABI type
    =soft                                                               -   Soft float ABI (implied by -soft-float)
    =hard                                                               -   Hard float ABI (uses FP registers)
  --force-dwarf-frame-section                                           - Always emit a debug frame section.
  --fp-contract=<value>                                                 - Enable aggressive formation of fused FP ops
    =fast                                                               -   Fuse FP ops whenever profitable
    =on                                                                 -   Only fuse 'blessed' FP ops.
    =off                                                                -   Only fuse FP ops when the result won't be affected.
  --frame-pointer=<value>                                               - Specify frame pointer elimination optimization
    =all                                                                -   Disable frame pointer elimination
    =non-leaf                                                           -   Disable frame pointer elimination for non-leaf frame
    =none                                                               -   Enable frame pointer elimination
  --fs-profile-debug-bw-threshold=<uint>                                - Only show debug message if the source branch weight is greater  than this value.
  --fs-profile-debug-prob-diff-threshold=<uint>                         - Only show debug message if the branch probility is greater than this value (in percentage).
  --function-sections                                                   - Emit functions into separate sections
  --generate-merged-base-profiles                                       - When generating nested context-sensitive profiles, always generate extra base profile for function with all its context profiles merged into it.
  --hash-based-counter-split                                            - Rename counter variable of a comdat function based on cfg hash
  --hot-cold-split                                                      - Enable hot-cold splitting pass
  --ignore-xcoff-visibility                                             - Not emit the visibility attribute for asm in AIX OS or give all symbols 'unspecified' visibility in XCOFF object file
  --import-all-index                                                    - Import all external functions in index.
  --incremental-linker-compatible                                       - When used with filetype=obj, emit an object file which can be used with an incremental linker
  --instcombine-code-sinking                                            - Enable code sinking
  --instcombine-guard-widening-window=<uint>                            - How wide an instruction window to bypass looking for another guard
  --instcombine-max-iterations=<uint>                                   - Limit the maximum number of instruction combining iterations
  --instcombine-max-num-phis=<uint>                                     - Maximum number phis to handle in intptr/ptrint folding
  --instcombine-max-sink-users=<uint>                                   - Maximum number of undroppable users for instruction sinking
  --instcombine-maxarray-size=<uint>                                    - Maximum array size considered when doing a combine
  --instcombine-negator-enabled                                         - Should we attempt to sink negations?
  --instcombine-negator-max-depth=<uint>                                - What is the maximal lookup depth when trying to check for viability of negation sinking.
  --instrprof-atomic-counter-update-all                                 - Make all profile counter updates atomic (for testing only)
  --internalize-public-api-file=<filename>                              - A file containing list of symbol names to preserve
  --internalize-public-api-list=<list>                                  - A list of symbol names to preserve
  --iterative-counter-promotion                                         - Allow counter promotion across the whole loop nest.
  --load=<pluginfilename>                                               - Load the specified plugin
  --load-pass-plugin=<string>                                           - Load passes from plugin library
  --lower-global-dtors-via-cxa-atexit                                   - Lower llvm.global_dtors (global destructors) via __cxa_atexit
  --march=<string>                                                      - Architecture to generate code for (see --version)
  --matrix-default-layout=<value>                                       - Sets the default matrix layout
    =column-major                                                       -   Use column-major layout
    =row-major                                                          -   Use row-major layout
  --matrix-print-after-transpose-opt                                    - 
  --mattr=<a1,+a2,-a3,...>                                              - Target specific attributes (-mattr=help for details)
  --max-counter-promotions=<int>                                        - Max number of allowed counter promotions
  --max-counter-promotions-per-loop=<uint>                              - Max number counter promotions per loop to avoid increasing register pressure too much
  --mc-relax-all                                                        - When used with filetype=obj, relax all fixups in the emitted object file
  --mcpu=<cpu-name>                                                     - Target a specific cpu type (-mcpu=help for details)
  --meabi=<value>                                                       - Set EABI type (default depends on triple):
    =default                                                            -   Triple default EABI version
    =4                                                                  -   EABI version 4
    =5                                                                  -   EABI version 5
    =gnu                                                                -   EABI GNU
  --mir-strip-debugify-only                                             - Should mir-strip-debug only strip debug info from debugified modules by default
  --misexpect-tolerance=<uint>                                          - Prevents emiting diagnostics when profile counts are within N% of the threshold..
  --module-hash                                                         - Emit module hash
  --module-summary                                                      - Emit module summary index
  --mtriple=<string>                                                    - Override target triple for module
  --no-deprecated-warn                                                  - Suppress all deprecated warnings
  --no-discriminators                                                   - Disable generation of discriminator information.
  --no-type-check                                                       - Suppress type errors (Wasm)
  --no-warn                                                             - Suppress all warnings
  --no-xray-index                                                       - Don't emit xray_fn_idx section
  --nozero-initialized-in-bss                                           - Don't place zero-initialized symbols into bss section
  -o <filename>                                                         - Override output filename
  --opaque-pointers                                                     - Use opaque pointers
  --pass-remarks-filter=<regex>                                         - Only record optimization remarks from passes whose names match the given regular expression
  --pass-remarks-format=<format>                                        - The format used for serializing remarks (default: YAML)
  --pass-remarks-output=<filename>                                      - Output filename for pass remarks
  --passes=<string>                                                     - A textual description of the pass pipeline. To have analysis passes available before a certain pass, add 'require<foo-analysis>'.
  --poison-checking-function-local                                      - Check that returns are non-poison (for testing)
  --print-breakpoints-for-testing                                       - Print select breakpoints location for testing
  --print-passes                                                        - Print available passes that can be specified in -passes=foo and exit
  --print-pipeline-passes                                               - Print a '-passes' compatible string describing the pipeline (best-effort only).
  --relax-elf-relocations                                               - Emit GOTPCRELX/REX_GOTPCRELX instead of GOTPCREL on x86-64 ELF
  --relocation-model=<value>                                            - Choose relocation model
    =static                                                             -   Non-relocatable code
    =pic                                                                -   Fully relocatable, position independent code
    =dynamic-no-pic                                                     -   Relocatable external references, non-relocatable code
    =ropi                                                               -   Code and read-only data relocatable, accessed PC-relative
    =rwpi                                                               -   Read-write data relocatable, accessed relative to static base
    =ropi-rwpi                                                          -   Combination of ropi and rwpi
  --runtime-counter-relocation                                          - Enable relocating counters at runtime.
  --safepoint-ir-verifier-print-only                                    - 
  --sample-profile-check-record-coverage=<N>                            - Emit a warning if less than N% of records in the input profile are matched to the IR.
  --sample-profile-check-sample-coverage=<N>                            - Emit a warning if less than N% of samples in the input profile are matched to the IR.
  --sample-profile-max-propagate-iterations=<uint>                      - Maximum number of iterations to go through when propagating sample block/edge weights through the CFG.
  --skip-ret-exit-block                                                 - Suppress counter promotion if exit blocks contain ret.
  --speculative-counter-promotion-max-exiting=<uint>                    - The max number of exiting blocks of a loop to allow  speculative counter promotion
  --speculative-counter-promotion-to-loop                               - When the option is false, if the target block is in a loop, the promotion will be disallowed unless the promoted counter  update can be further/iteratively promoted into an acyclic  region.
  --split-machine-functions                                             - Split out cold basic blocks from machine functions based on profile information
  --stack-size-section                                                  - Emit a section containing stack size metadata
  --stack-symbol-ordering                                               - Order local stack symbols.
  --stackrealign                                                        - Force align the stack to the minimum alignment
  --strict-dwarf                                                        - use strict dwarf
  --strip-debug                                                         - Strip debugger symbol info from translation unit
  --strip-named-metadata                                                - Strip module-level named metadata
  --summary-file=<string>                                               - The summary file to use for function importing.
  --swift-async-fp=<value>                                              - Determine when the Swift async frame pointer should be set
    =auto                                                               -   Determine based on deployment target
    =always                                                             -   Always set the bit
    =never                                                              -   Never set the bit
  --tailcallopt                                                         - Turn fastcc calls into tail calls by (potentially) changing ABI.
  --thin-link-bitcode-file=<filename>                                   - A file in which to write minimized bitcode for the thin link only
  --thinlto-bc                                                          - Write output as ThinLTO-ready bitcode
  --thinlto-split-lto-unit                                              - Enable splitting of a ThinLTO LTOUnit
  --thread-model=<value>                                                - Choose threading model
    =posix                                                              -   POSIX thread model
    =single                                                             -   Single thread model
  --time-trace                                                          - Record time trace
  --time-trace-file=<filename>                                          - Specify time trace file destination
  --tls-size=<uint>                                                     - Bit size of immediate TLS offsets
  --type-based-intrinsic-cost                                           - Calculate intrinsics cost based only on argument types
  --unique-basic-block-section-names                                    - Give unique names to every basic block section
  --unique-section-names                                                - Give unique names to every section
  --use-ctors                                                           - Use .ctors instead of .init_array.
  --vec-extabi                                                          - Enable the AIX Extended Altivec ABI.
  --verify-debuginfo-preserve                                           - Start the pipeline with collecting and end it with checking of debug info preservation.
  --verify-di-preserve-export=<filename>                                - Export debug info preservation failures into specified (JSON) file (should be abs path as we use append mode to insert new JSON objects)
  --verify-each                                                         - Verify after each transform
  --verify-each-debuginfo-preserve                                      - Start each pass with collecting and end it with checking of debug info preservation.
  --verify-legalizer-debug-locs=<value>                                 - Verify that debug locations are handled
    =none                                                               -   No verification
    =legalizations                                                      -   Verify legalizations
    =legalizations+artifactcombiners                                    -   Verify legalizations and artifact combines
  --verify-region-info                                                  - Verify region info (time consuming)
  --vp-counters-per-site=<number>                                       - The average number of profile counters allocated per value profiling site.
  --vp-static-alloc                                                     - Do static counter allocation for value profiler
  --x86-align-branch=<string>                                           - Specify types of branches to align (plus separated list of types):
                                                                          jcc      indicates conditional jumps
                                                                          fused    indicates fused conditional jumps
                                                                          jmp      indicates direct unconditional jumps
                                                                          call     indicates direct and indirect calls
                                                                          ret      indicates rets
                                                                          indirect indicates indirect unconditional jumps
  --x86-align-branch-boundary=<uint>                                    - Control how the assembler should align branches with NOP. If the boundary's size is not 0, it should be a power of 2 and no less than 32. Branches will be aligned to prevent from being across or against the boundary of specified size. The default value 0 does not align branches.
  --x86-branches-within-32B-boundaries                                  - Align selected instructions to mitigate negative performance impact of Intel's micro code update for errata skx102.  May break assumptions about labels corresponding to particular instructions, and should be used with caution.
  --x86-pad-max-prefix-size=<uint>                                      - Maximum number of prefixes to use for padding
  --xcoff-traceback-table                                               - Emit the XCOFF traceback table

Generic Options:

  --help                                                                - Display available options (--help-hidden for more)
  --help-list                                                           - Display list of available options (--help-list-hidden for more)
  --version                                                             - Display the version of this program


  ## xliinx-llvm/opt:
  OVERVIEW: llvm .bc -> .bc modular optimizer and analysis printer

USAGE: opt [subcommand] [options] <input bitcode file>

OPTIONS:

General options:

  -O1                                                            - Optimization level 1. Similar to clang -O1
  -O2                                                            - Optimization level 2. Similar to clang -O2
  -O3                                                            - Optimization level 3. Similar to clang -O3
  -Os                                                            - Like -O2 with extra optimizations for size. Similar to clang -Os
  -Oz                                                            - Like -Os but reduces code size further. Similar to clang -Oz
  -S                                                             - Write output as LLVM assembly
  -aarch64-neon-syntax                                           - Choose style of NEON code to emit from AArch64 backend:
    =generic                                                     -   Emit generic NEON assembly
    =apple                                                       -   Emit Apple-style NEON assembly
  -analyze                                                       - Only perform analysis, no optimization
  -asm-instrumentation                                           - Instrumentation of inline assembly and assembly source files
    =none                                                        -   no instrumentation at all
    =address                                                     -   instrument instructions with memory arguments
  -asm-show-inst                                                 - Emit internal instruction representation to assembly file
  Optimizations available:
    -aa                                                          - Function Alias Analysis Results
    -aa-eval                                                     - Exhaustive Alias Analysis Precision Evaluator
    -aarch64-expand-pseudo                                       - AArch64 pseudo instruction expansion pass
    -adce                                                        - Aggressive Dead Code Elimination
    -add-discriminators                                          - Add DWARF path discriminators
    -alignment-from-assumptions                                  - Alignment from assumptions
    -always-inline                                               - Inliner for always_inline functions
    -argpromotion                                                - Promote 'by reference' arguments to scalars
    -arm-ldst-opt                                                - ARM load / store optimization pass
    -arm-prera-ldst-opt                                          - ARM pre- register allocation load / store optimization pass
    -asan                                                        - AddressSanitizer: detects use-after-free and out-of-bounds bugs.
    -asan-module                                                 - AddressSanitizer: detects use-after-free and out-of-bounds bugs.ModulePass
    -assumption-cache-tracker                                    - Assumption Cache Tracker
    -atomic-expand                                               - Expand Atomic instructions
    -barrier                                                     - A No-Op Barrier Pass
    -basicaa                                                     - Basic Alias Analysis (stateless AA impl)
    -basiccg                                                     - CallGraph Construction
    -bb-vectorize                                                - Basic-Block Vectorization
    -bdce                                                        - Bit-Tracking Dead Code Elimination
    -block-freq                                                  - Block Frequency Analysis
    -bounds-checking                                             - Run-time bounds checking
    -branch-prob                                                 - Branch Probability Analysis
    -break-crit-edges                                            - Break critical edges in CFG
    -cfl-anders-aa                                               - Inclusion-Based CFL Alias Analysis
    -cfl-steens-aa                                               - Unification-Based CFL Alias Analysis
    -codegenprepare                                              - Optimize for code generation
    -consthoist                                                  - Constant Hoisting
    -constmerge                                                  - Merge Duplicate Global Constants
    -constprop                                                   - Simple constant propagation
    -correlated-propagation                                      - Value Propagation
    -cost-model                                                  - Cost Model Analysis
    -cross-dso-cfi                                               - Cross-DSO CFI
    -da                                                          - Dependence Analysis
    -dce                                                         - Dead Code Elimination
    -deadargelim                                                 - Dead Argument Elimination
    -deadarghaX0r                                                - Dead Argument Hacking (BUGPOINT USE ONLY; DO NOT USE)
    -delinearize                                                 - Delinearization
    -demanded-bits                                               - Demanded bits analysis
    -dfsan                                                       - DataFlowSanitizer: dynamic data flow analysis.
    -die                                                         - Dead Instruction Elimination
    -divergence                                                  - Divergence Analysis
    -domfrontier                                                 - Dominance Frontier Construction
    -domtree                                                     - Dominator Tree Construction
    -dot-callgraph                                               - Print call graph to 'dot' file
    -dot-cfg                                                     - Print CFG of function to 'dot' file
    -dot-cfg-only                                                - Print CFG of function to 'dot' file (with no function bodies)
    -dot-dom                                                     - Print dominance tree of function to 'dot' file
    -dot-dom-only                                                - Print dominance tree of function to 'dot' file (with no function bodies)
    -dot-postdom                                                 - Print postdominance tree of function to 'dot' file
    -dot-postdom-only                                            - Print postdominance tree of function to 'dot' file (with no function bodies)
    -dot-regions                                                 - Print regions of function to 'dot' file
    -dot-regions-only                                            - Print regions of function to 'dot' file (with no function bodies)
    -dse                                                         - Dead Store Elimination
    -dwarfehprepare                                              - Prepare DWARF exceptions
    -early-cse                                                   - Early CSE
    -elim-avail-extern                                           - Eliminate Available Externally Globals
    -esan                                                        - EfficiencySanitizer: finds performance issues.
    -external-aa                                                 - External Alias Analysis
    -extract-blocks                                              - Extract Basic Blocks From Module (for bugpoint use)
    -flattencfg                                                  - Flatten the CFG
    -float2int                                                   - Float to int
    -forceattrs                                                  - Force set function attributes
    -function-import                                             - Summary Based Function Import
    -functionattrs                                               - Deduce function attributes
    -global-merge                                                - Merge global variables
    -globaldce                                                   - Dead Global Elimination
    -globalopt                                                   - Global Variable Optimizer
    -globals-aa                                                  - Globals Alias Analysis
    -guard-widening                                              - Widen guards
    -gvn                                                         - Global Value Numbering
    -gvn-hoist                                                   - Early GVN Hoisting of Expressions
    -indvars                                                     - Induction Variable Simplification
    -inferattrs                                                  - Infer set function attributes
    -inline                                                      - Function Integration/Inlining
    -insert-gcov-profiling                                       - Insert instrumentation for GCOV profiling
    -instcombine                                                 - Combine redundant instructions
    -instcount                                                   - Counts the various types of Instructions
    -instnamer                                                   - Assign names to anonymous instructions
    -instrprof                                                   - Frontend instrumentation-based coverage lowering.
    -instsimplify                                                - Remove redundant instructions
    -interleaved-access                                          - Lower interleaved memory accesses to target specific intrinsics
    -internalize                                                 - Internalize Global Symbols
    -intervals                                                   - Interval Partition Construction
    -ipconstprop                                                 - Interprocedural constant propagation
    -ipsccp                                                      - Interprocedural Sparse Conditional Constant Propagation
    -irce                                                        - Inductive range check elimination
    -iv-users                                                    - Induction Variable Users
    -jump-threading                                              - Jump Threading
    -lazy-block-freq                                             - Lazy Block Frequency Analysis
    -lazy-value-info                                             - Lazy Value Information Analysis
    -lcssa                                                       - Loop-Closed SSA Form Pass
    -licm                                                        - Loop Invariant Code Motion
    -lint                                                        - Statically lint-checks LLVM IR
    -load-combine                                                - Combine Adjacent Loads
    -load-store-vectorizer                                       - Vectorize load and store instructions
    -loop-accesses                                               - Loop Access Analysis
    -loop-data-prefetch                                          - Loop Data Prefetch
    -loop-deletion                                               - Delete dead loops
    -loop-distribute                                             - Loop Distribition
    -loop-extract                                                - Extract loops into new functions
    -loop-extract-single                                         - Extract at most one loop into a new function
    -loop-idiom                                                  - Recognize loop idioms
    -loop-instsimplify                                           - Simplify instructions in loops
    -loop-interchange                                            - Interchanges loops for cache reuse
    -loop-load-elim                                              - Loop Load Elimination
    -loop-reduce                                                 - Loop Strength Reduction
    -loop-reroll                                                 - Reroll loops
    -loop-rotate                                                 - Rotate Loops
    -loop-simplify                                               - Canonicalize natural loops
    -loop-simplifycfg                                            - Simplify loop CFG
    -loop-unroll                                                 - Unroll loops
    -loop-unswitch                                               - Unswitch loops
    -loop-vectorize                                              - Loop Vectorization
    -loop-versioning                                             - Loop Versioning
    -loop-versioning-licm                                        - Loop Versioning For LICM
    -loops                                                       - Natural Loop Information
    -lower-expect                                                - Lower 'expect' Intrinsics
    -lower-guard-intrinsic                                       - Lower the guard intrinsic to normal control flow
    -loweratomic                                                 - Lower atomic intrinsics to non-atomic form
    -lowerinvoke                                                 - Lower invoke and unwind, for unwindless code generators
    -lowerswitch                                                 - Lower SwitchInst's to branches
    -lowertypetests                                              - Lower type metadata
    -mem2reg                                                     - Promote Memory to Register
    -memcpyopt                                                   - MemCpy Optimization
    -memdep                                                      - Memory Dependence Analysis
    -memoryssa                                                   - Memory SSA
    -mergefunc                                                   - Merge Functions
    -mergereturn                                                 - Unify function exit nodes
    -metarenamer                                                 - Assign new names to everything
    -mldst-motion                                                - MergedLoadStoreMotion
    -module-debuginfo                                            - Decodes module-level debug info
    -module-summary-analysis                                     - Module Summary Analysis
    -msan                                                        - MemorySanitizer: detects uninitialized reads.
    -name-anon-functions                                         - Provide a name to nameless functions
    -nary-reassociate                                            - Nary reassociation
    -objc-arc                                                    - ObjC ARC optimization
    -objc-arc-aa                                                 - ObjC-ARC-Based Alias Analysis
    -objc-arc-apelim                                             - ObjC ARC autorelease pool elimination
    -objc-arc-contract                                           - ObjC ARC contraction
    -objc-arc-expand                                             - ObjC ARC expansion
    -opt-remark-emitter                                          - Optimization Remark Emitter
    -pa-eval                                                     - Evaluate ProvenanceAnalysis on all pairs
    -partial-inliner                                             - Partial Inliner
    -partially-inline-libcalls                                   - Partially inline calls to library functions
    -pgo-icall-prom                                              - Use PGO instrumentation profile to promote indirect calls to direct calls.
    -pgo-instr-gen                                               - PGO instrumentation.
    -pgo-instr-use                                               - Read PGO instrumentation profile.
    -place-backedge-safepoints-impl                              - Place Backedge Safepoints
    -place-safepoints                                            - Place Safepoints
    -postdomtree                                                 - Post-Dominator Tree Construction
    -pre-isel-intrinsic-lowering                                 - Pre-ISel Intrinsic Lowering
    -print-alias-sets                                            - Alias Set Printer
    -print-bb                                                    - Print BB to stderr
    -print-callgraph                                             - Print a call graph
    -print-callgraph-sccs                                        - Print SCCs of the Call Graph
    -print-cfg-sccs                                              - Print SCCs of each function CFG
    -print-dom-info                                              - Dominator Info Printer
    -print-externalfnconstants                                   - Print external fn callsites passed constants
    -print-function                                              - Print function to stderr
    -print-memdeps                                               - Print MemDeps of function
    -print-memderefs                                             - Memory Dereferenciblity of pointers in function
    -print-memoryssa                                             - Memory SSA Printer
    -print-module                                                - Print module to stderr
    -profile-summary-info                                        - Profile summary info
    -prune-eh                                                    - Remove unused exception handling info
    -reassociate                                                 - Reassociate expressions
    -reg2mem                                                     - Demote all values to stack slots
    -regions                                                     - Detect single entry single exit regions
    -rewrite-statepoints-for-gc                                  - Make relocations explicit at statepoints
    -rewrite-symbols                                             - Rewrite Symbols
    -rpo-functionattrs                                           - Deduce function attributes in RPO
    -safe-stack                                                  - Safe Stack instrumentation pass
    -sample-profile                                              - Sample Profile loader
    -sancov                                                      - SanitizerCoverage: TODO.ModulePass
    -scalar-evolution                                            - Scalar Evolution Analysis
    -scalarizer                                                  - Scalarize vector operations
    -sccp                                                        - Sparse Conditional Constant Propagation
    -scev-aa                                                     - ScalarEvolution-based Alias Analysis
    -scoped-noalias                                              - Scoped NoAlias Alias Analysis
    -separate-const-offset-from-gep                              - Split GEPs to a variadic base and a constant offset for better CSE
    -simplifycfg                                                 - Simplify the CFG
    -sink                                                        - Code sinking
    -sjljehprepare                                               - Prepare SjLj exceptions
    -slp-vectorizer                                              - SLP Vectorizer
    -slsr                                                        - Straight line strength reduction
    -speculative-execution                                       - Speculatively execute instructions
    -sroa                                                        - Scalar Replacement Of Aggregates
    -strip                                                       - Strip all symbols from a module
    -strip-dead-debug-info                                       - Strip debug info for unused symbols
    -strip-dead-prototypes                                       - Strip Unused Function Prototypes
    -strip-debug-declare                                         - Strip all llvm.dbg.declare intrinsics
    -strip-nondebug                                              - Strip all symbols, except dbg symbols, from a module
    -structurizecfg                                              - Structurize the CFG
    -tailcallelim                                                - Tail Call Elimination
    -targetlibinfo                                               - Target Library Information
    -tbaa                                                        - Type-Based Alias Analysis
    -tsan                                                        - ThreadSanitizer: detects data races.
    -tti                                                         - Target Transform Information
    -unreachableblockelim                                        - Remove unreachable blocks from the CFG
    -verify                                                      - Module Verifier
    -view-callgraph                                              - View call graph
    -view-cfg                                                    - View CFG of function
    -view-cfg-only                                               - View CFG of function (with no function bodies)
    -view-dom                                                    - View dominance tree of function
    -view-dom-only                                               - View dominance tree of function (with no function bodies)
    -view-postdom                                                - View postdominance tree of function
    -view-postdom-only                                           - View postdominance tree of function (with no function bodies)
    -view-regions                                                - View regions of function
    -view-regions-only                                           - View regions of function (with no function bodies)
    -wholeprogramdevirt                                          - Whole program devirtualization
    -winehprepare                                                - Prepare Windows exceptions
    -x86-fixup-bw-insts                                          - X86 Byte/Word Instruction Fixup
    -x86-winehstate                                              - Insert stores for EH state numbers
  -bounds-checking-single-trap                                   - Use one trap block per function
  -code-model                                                    - Choose code model
    =default                                                     -   Target default code model
    =small                                                       -   Small code model
    =kernel                                                      -   Kernel code model
    =medium                                                      -   Medium code model
    =large                                                       -   Large code model
  -codegen-opt-level=<uint>                                      - Override optimization level for codegen hooks
  -data-sections                                                 - Emit data into separate sections
  -debugger-tune                                                 - Tune debug info for a particular debugger
    =gdb                                                         -   gdb
    =lldb                                                        -   lldb
    =sce                                                         -   SCE targets (e.g. PS4)
  -default-data-layout=<layout-string>                           - data layout string to use if not specified by module
  -disable-debug-info-type-map                                   - Don't use a uniquing type map for debug info
  -disable-fp-elim                                               - Disable frame pointer elimination optimization
  -disable-inlining                                              - Do not run the inliner pass
  -disable-loop-unrolling                                        - Disable loop unrolling in all relevant passes
  -disable-loop-vectorization                                    - Disable the loop vectorization pass
  -disable-opt                                                   - Do not run any optimization passes
  -disable-simplify-libcalls                                     - Disable simplify-libcalls
  -disable-slp-vectorization                                     - Disable the slp vectorization pass
  -disable-spill-fusing                                          - Disable fusing of spill code into instructions
  -disable-tail-calls                                            - Never emit tail calls
  -dwarf-version=<int>                                           - Dwarf version
  -emulated-tls                                                  - Use emulated TLS model
  -enable-fp-mad                                                 - Enable less precise MAD instructions to be generated
  -enable-implicit-null-checks                                   - Fold null checks into faulting memory operations
  -enable-load-pre                                               - 
  -enable-name-compression                                       - Enable name string compression
  -enable-no-infs-fp-math                                        - Enable FP math optimizations that assume no +-Infs
  -enable-no-nans-fp-math                                        - Enable FP math optimizations that assume no NaNs
  -enable-objc-arc-opts                                          - enable/disable all ARC Optimizations
  -enable-scoped-noalias                                         - 
  -enable-tbaa                                                   - 
  -enable-unsafe-fp-math                                         - Enable optimizations that may decrease FP precision
  -exception-model                                               - exception model
    =default                                                     -   default exception handling model
    =dwarf                                                       -   DWARF-like CFI based exception handling
    =sjlj                                                        -   SjLj exception handling
    =arm                                                         -   ARM EHABI exceptions
    =wineh                                                       -   Windows exception model
  -exhaustive-register-search                                    - Exhaustive Search for registers bypassing the depth and interference cutoffs of last chance recoloring
  -expensive-combines                                            - Enable expensive instruction combines
  -f                                                             - Enable binary output on terminals
  -fatal-warnings                                                - Treat warnings as errors
  -filetype                                                      - Choose a file type (not all types are supported by all targets):
    =asm                                                         -   Emit an assembly ('.s') file
    =obj                                                         -   Emit a native object ('.o') file
    =null                                                        -   Emit nothing, for performance testing
  -filter-print-funcs=<function names>                           - Only print IR for functions whose name match this for all print-[before|after][-all] options
  -float-abi                                                     - Choose float ABI type
    =default                                                     -   Target default float ABI type
    =soft                                                        -   Soft float ABI (implied by -soft-float)
    =hard                                                        -   Hard float ABI (uses FP registers)
  -fp-contract                                                   - Enable aggressive formation of fused FP ops
    =fast                                                        -   Fuse FP ops whenever profitable
    =on                                                          -   Only fuse 'blessed' FP ops.
    =off                                                         -   Only fuse FP ops when the result won't be affected.
  -function-sections                                             - Emit functions into separate sections
  -funit-at-a-time                                               - Enable IPO. This corresponds to gcc's -funit-at-a-time
  -imp-null-check-page-size=<int>                                - The page size of the target in bytes
  -incremental-linker-compatible                                 - When used with filetype=obj, emit an object file which can be used with an incremental linker
  -internalize-public-api-file=<filename>                        - A file containing list of symbol names to preserve
  -internalize-public-api-list=<list>                            - A list of symbol names to preserve
  -join-liveintervals                                            - Coalesce copies (default=true)
  -jump-table-type                                               - Choose the type of Jump-Instruction Table for jumptable.
    =single                                                      -   Create a single table for all jumptable functions
    =arity                                                       -   Create one table per number of parameters.
    =simplified                                                  -   Create one table per simplified function type.
    =full                                                        -   Create one table per unique function type.
  -limit-float-precision=<uint>                                  - Generate low-precision inline sequences for some float libcalls
  -load=<pluginfilename>                                         - Load the specified plugin
  -march=<string>                                                - Architecture to generate code for (see --version)
  -mattr=<a1,+a2,-a3,...>                                        - Target specific attributes (-mattr=help for details)
  -mc-relax-all                                                  - When used with filetype=obj, relax all fixups in the emitted object file
  -mcpu=<cpu-name>                                               - Target a specific cpu type (-mcpu=help for details)
  -meabi                                                         - Set EABI type (default depends on triple):
    =default                                                     -   Triple default EABI version
    =4                                                           -   EABI version 4
    =5                                                           -   EABI version 5
    =gnu                                                         -   EABI GNU
  -module-hash                                                   - Emit module hash
  -module-summary                                                - Emit module summary index
  -mtriple=<string>                                              - Override target triple for module
  -no-discriminators                                             - Disable generation of discriminator information.
  -no-warn                                                       - Suppress all warnings
  -nozero-initialized-in-bss                                     - Don't place zero-initialized symbols into bss section
  -o=<filename>                                                  - Override output filename
  -p                                                             - Print module after each transformation
  -print-after-all                                               - Print IR after each pass
  -print-before-all                                              - Print IR before each pass
  -print-breakpoints-for-testing                                 - Print select breakpoints location for testing
  -print-machineinstrs=<pass-name>                               - Print machine instrs
  -recip=<all,none,default,divf,!vec-sqrtd,vec-divd:0,sqrt:9...> - Choose reciprocal operation types and parameters.
  -regalloc                                                      - Register allocator to use
    =default                                                     -   pick register allocator based on -O option
    =pbqp                                                        -   PBQP register allocator
    =fast                                                        -   fast register allocator
    =greedy                                                      -   greedy register allocator
  -relocation-model                                              - Choose relocation model
    =static                                                      -   Non-relocatable code
    =pic                                                         -   Fully relocatable, position independent code
    =dynamic-no-pic                                              -   Relocatable external references, non-relocatable code
  -rewrite-map-file=<filename>                                   - Symbol Rewrite Map
  -rng-seed=<seed>                                               - Seed for the random number generator
  -sample-profile-check-record-coverage=<N>                      - Emit a warning if less than N% of records in the input profile are matched to the IR.
  -sample-profile-check-sample-coverage=<N>                      - Emit a warning if less than N% of samples in the input profile are matched to the IR.
  -sample-profile-inline-hot-threshold=<N>                       - Inlined functions that account for more than N% of all samples collected in the parent function, will be inlined again.
  -sample-profile-max-propagate-iterations=<uint>                - Maximum number of iterations to go through when propagating sample block/edge weights through the CFG.
  -stack-alignment=<uint>                                        - Override default stack alignment
  -stack-symbol-ordering                                         - Order local stack symbols.
  -stackmap-version=<int>                                        - Specify the stackmap encoding version (default = 1)
  -stackrealign                                                  - Force align the stack to the minimum alignment
  -start-after=<pass-name>                                       - Resume compilation after a specific pass
  -static-func-full-module-prefix                                - Use full module build paths in the profile counter names for static functions.
  -stats                                                         - Enable statistics output from program (available with Asserts)
  -stats-json                                                    - Display statistics as json data
  -std-link-opts                                                 - Include the standard link time optimizations
  -stop-after=<pass-name>                                        - Stop compilation after a specific pass
  -strip-debug                                                   - Strip debugger symbol info from translation unit
  -summary-file=<string>                                         - The summary file to use for function importing.
  -tailcallopt                                                   - Turn fastcc calls into tail calls by (potentially) changing ABI.
  -thread-model                                                  - Choose threading model
    =posix                                                       -   POSIX thread model
    =single                                                      -   Single thread model
  -time-passes                                                   - Time each pass, printing elapsed time for each on exit
  -unique-section-names                                          - Give unique names to every section
  -use-ctors                                                     - Use .ctors instead of .init_array.
  -verify-debug-info                                             - 
  -verify-dom-info                                               - Verify dominator info (time consuming)
  -verify-each                                                   - Verify after each transform
  -verify-loop-info                                              - Verify loop info (time consuming)
  -verify-machine-dom-info                                       - Verify machine dominator info (time consuming)
  -verify-regalloc                                               - Verify during register allocation
  -verify-region-info                                            - Verify region info (time consuming)
  -verify-scev                                                   - Verify ScalarEvolution's backedge taken counts (slow)
  -verify-scev-maps                                              - Verify no dangling value in ScalarEvolution's ExprValueMap (slow)
  -vp-counters-per-site=<number>                                 - The average number of profile counters allocated per value profiling site.
  -vp-static-alloc                                               - Do static counter allocation for value profiler
  -x86-asm-syntax                                                - Choose style of code to emit from X86 backend:
    =att                                                         -   Emit AT&T-style assembly
    =intel                                                       -   Emit Intel-style assembly

Generic Options:

  -help                                                          - Display available options (-help-hidden for more)
  -help-list                                                     - Display list of available options (-help-list-hidden for more)
  -version                                                       - Display the version of this program