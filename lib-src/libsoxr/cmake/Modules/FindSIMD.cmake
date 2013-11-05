# - Finds SIMD support
#
# The following variables are set:
#   SIMD_C_FLAGS - flags to add to the C compiler for this package.
#   SIMD_FOUND - true if support for this package is found.
#
#=============================================================================
# Based on FindOpenMP.cmake, which is:
#
# Copyright 2009 Kitware, Inc.
# Copyright 2008-2009 André Rigland Brodtkorb <Andre.Brodtkorb@ifi.uio.no>
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#  * Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#
#  * Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
#  * The names of Kitware, Inc., the Insight Consortium, or the names of
#    any consortium members, or of any contributors, may not be used to
#    endorse or promote products derived from this software without
#    specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

include (CheckCSourceCompiles)
include (FindPackageHandleStandardArgs)

set (SIMD_C_FLAG_CANDIDATES
  # Microsoft Visual Studio x64
  " "
  # Microsoft Visual Studio x86
  "/arch:SSE /fp:fast -D__SSE__"
  # Gnu
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
