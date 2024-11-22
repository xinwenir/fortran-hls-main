#!/bin/bash
rm -rf fortran_hls.egg-info
rm -rf Makefile

# clean venv
rm -rf venv

# clean the llvm-passes-f18 file
cd llvm-passes-f18
rm -rf CMakeFiles cmake_install.cmake CMakeCache.txt .gitignore Makefile

cd extract_subroutines
rm -rf CMakeFiles cmake_install.cmake Makefile

cd ../set_pragma_metadata
rm -rf CMakeFiles cmake_install.cmake Makefile

cd ../vxxdowngrader
rm -rf CMakeFiles cmake_install.cmake  .gitignore Makefile

cd ../build-v7
rm -rf CMakeFiles cmake_install.cmake CMakeCache.txt set_pragma_metadata

cd ../build-v16
rm -rf CMakeFiles cmake_install.cmake CMakeCache.txt extract_subroutines vxxdowngrader
