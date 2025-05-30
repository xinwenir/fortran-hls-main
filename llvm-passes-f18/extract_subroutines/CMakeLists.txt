#***************************************************************
#   Copyright 2023 Hewlett Packard Enterprise Development LP.
#***************************************************************

add_library(ExtractFunctions MODULE
    # List your source files here.
    extract_subroutines.cpp
)


# Use C++11 to compile your pass (i.e., supply -std=c++11).
target_compile_features(ExtractFunctions PRIVATE cxx_range_for cxx_auto_type)

set_target_properties(ExtractFunctions PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED ON
    CXX_EXTENSIONS OFF
)

# LLVM is (typically) built with no C++ RTTI. We need to match that;
# otherwise, we'll get linker errors about missing RTTI data.
set_target_properties(ExtractFunctions PROPERTIES
    COMPILE_FLAGS "-fno-rtti"
)

# Get proper shared-library behavior (where symbols are not necessarily
# resolved when the shared library is linked) on OS X.
if(APPLE)
    set_target_properties(ExtractFunctions PROPERTIES
        LINK_FLAGS "-undefined dynamic_lookup"
    )
endif(APPLE)
