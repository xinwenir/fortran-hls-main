#***************************************************************
#   Copyright 2023 Hewlett Packard Enterprise Development LP.
#***************************************************************

add_library(SetPragmaMetadata MODULE
    # List your source files here.
    set_pragma_metadata.cpp
)
# Ensure the library is built with c++17 standrad.
#set_target_properties(SetPragmaMetadata PROPERTIES
#    CXX_STANDARD 17           # Use c++17
#    CXX_STANDARD_REQUIRED ON  # Require c++17, no fallback
#    CXX_EXTENSIONS OFF        # Disable compiler-specific extensions
#)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Use C++11 to compile your pass (i.e., supply -std=c++11).
target_compile_features(SetPragmaMetadata PRIVATE cxx_range_for cxx_auto_type)

# LLVM is (typically) built with no C++ RTTI. We need to match that;
# otherwise, we'll get linker errors about missing RTTI data.
set_target_properties(SetPragmaMetadata PROPERTIES
    COMPILE_FLAGS "-fno-rtti"
)

# Get proper shared-library behavior (where symbols are not necessarily
# resolved when the shared library is linked) on OS X.
if(APPLE)
    set_target_properties(SetPragmaMetadata PROPERTIES
        LINK_FLAGS "-undefined dynamic_lookup"
    )
endif(APPLE)

# Debug:
message(STATUS "C++ Compiler: ${CMAKE_CXX_COMPILER}")
message(STATUS "C++ Compiler Version: ${CMAKE_CXX_COMPILER_VERSION}")
message(STATUS "C++ Flags: ${CMAKE_CXX_FLAGS}")
