cd /home/zxw/fxx
git clone https://github.com/llvm/llvm-project.git
cd llvm-project

#rm -rf build
#mkdir build
#rm -rf install
#mkdir install

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