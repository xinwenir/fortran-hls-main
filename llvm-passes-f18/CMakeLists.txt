#***************************************************************
#   Copyright 2023 Hewlett Packard Enterprise Development LP.
#***************************************************************

cmake_minimum_required(VERSION 3.4)
project(fortran-hls-passes)

# we need LLVM_HOME in order to automatically set LLVM_DIR
if(NOT DEFINED ENV{LLVM_HOME})
    message(FATAL_ERROR "$LLVM_HOME is not defined")
else ()
    set(ENV{LLVM_DIR} $ENV{LLVM_HOME}/lib/cmake/llvm)
endif()

find_package(LLVM REQUIRED CONFIG)
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})
set(CMAKE_CXX_STANDARD 17)

if(V16)
    add_subdirectory(vxxdowngrader)  
    add_subdirectory(extract_subroutines)
elseif(V7)
    add_subdirectory(set_pragma_metadata)
endif()
