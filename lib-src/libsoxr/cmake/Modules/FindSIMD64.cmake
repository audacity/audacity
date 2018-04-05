# SoX Resampler Library       Copyright (c) 2007-16 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

# - Finds SIMD64 support
#
# The following variables are set:
#   SIMD64_C_FLAGS - flags to add to the C compiler for this package.
#   SIMD64_FOUND - true if support for this package is found.

if (DEFINED SIMD64_C_FLAGS OR CMAKE_SYSTEM_PROCESSOR MATCHES "^arm")
  set (TRIAL_C_FLAGS)
else ()
  set (TRIAL_C_FLAGS
    "-mavx" # Gcc
    "/arch:AVX" # MSVC
  )
  set (TEST_C_SOURCE "
    #ifndef __AVX__
      #error
    #endif
    #include <immintrin.h>
    int main() {return 0;}
    ")
endif ()

include (FindCFlags)

FindCFlags ("SIMD64" "FLOAT-64 (double-precision) SIMD vectorization"
  "${TRIAL_C_FLAGS}" "${TEST_C_SOURCE}")
