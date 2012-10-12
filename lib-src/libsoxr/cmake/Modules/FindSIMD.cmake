# - Finds SIMD support
#
# The following variables are set:
#   SIMD_C_FLAGS - flags to add to the C compiler for this package.
#   SIMD_FOUND - true if support for this package is found.

include (CheckCSourceCompiles)
include (FindPackageHandleStandardArgs)

set (SIMD_C_FLAG_CANDIDATES
  #Microsoft Visual Studio
  "/arch:SSE /fp:fast -D__SSE__"
  #Gnu
  "-msse -mfpmath=sse"
)

set (SIMD_C_TEST_SOURCE
"
#include <xmmintrin.h>
int main()
{
  __m128 a, b;
  float vals[4] = {0};
  a = _mm_loadu_ps (vals);
  b = a;
  b = _mm_add_ps (a,b);
  _mm_storeu_ps (vals,b);
  return 0;
}
")

if (DEFINED SIMD_C_FLAGS)
  set (SIMD_C_FLAG_CANDIDATES)
endif ()

foreach (FLAG ${SIMD_C_FLAG_CANDIDATES})
  set (SAFE_CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS}")
  set (CMAKE_REQUIRED_FLAGS "${FLAG}")
  unset (SIMD_FLAG_DETECTED CACHE)
  message (STATUS "Try SIMD C flag = [${FLAG}]")
  check_c_source_compiles ("${SIMD_C_TEST_SOURCE}" SIMD_FLAG_DETECTED)
  set (CMAKE_REQUIRED_FLAGS "${SAFE_CMAKE_REQUIRED_FLAGS}")
  if (SIMD_FLAG_DETECTED)
    set (SIMD_C_FLAGS_INTERNAL "${FLAG}")
    break ()
  endif ()
endforeach ()

set (SIMD_C_FLAGS "${SIMD_C_FLAGS_INTERNAL}"
  CACHE STRING "C compiler flags for SIMD vectorization")

find_package_handle_standard_args (SIMD DEFAULT_MSG SIMD_C_FLAGS SIMD_C_FLAGS)
mark_as_advanced (SIMD_C_FLAGS)
