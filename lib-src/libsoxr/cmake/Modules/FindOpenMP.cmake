# - Finds OpenMP support
# This module can be used to detect OpenMP support in a compiler.
# If the compiler supports OpenMP, the flags required to compile with
# openmp support are set.
#
# The following variables are set:
#   OpenMP_C_FLAGS - flags to add to the C compiler for OpenMP support
#   OPENMP_FOUND - true if openmp is detected
#
# Supported compilers can be found at http://openmp.org/wp/openmp-compilers/
#
# Modified for libsoxr not to rely on presence of C++ compiler.
#
#=============================================================================
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

set (OpenMP_C_FLAG_CANDIDATES
  #Gnu
  "-fopenmp"
  #Microsoft Visual Studio
  "/openmp"
  #Intel windows
  "-Qopenmp"
  #Intel
  "-openmp"
  #Empty, if compiler automatically accepts openmp
  " "
  #Sun
  "-xopenmp"
  #HP
  "+Oopenmp"
  #IBM XL C/c++
  "-qsmp"
  #Portland Group
  "-mp"
)

# sample openmp source code to test
set (OpenMP_C_TEST_SOURCE
"
#include <omp.h>
int main() {
#ifdef _OPENMP
  return 0;
#else
  breaks_on_purpose
#endif
}
")
# if these are set then do not try to find them again,
# by avoiding any try_compiles for the flags
if (DEFINED OpenMP_C_FLAGS)
  set (OpenMP_C_FLAG_CANDIDATES)
endif (DEFINED OpenMP_C_FLAGS)

# check c compiler
foreach (FLAG ${OpenMP_C_FLAG_CANDIDATES})
  set (SAFE_CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS}")
  set (CMAKE_REQUIRED_FLAGS "${FLAG}")
  unset (OpenMP_FLAG_DETECTED CACHE)
  message (STATUS "Try OpenMP C flag = [${FLAG}]")
  check_c_source_compiles ("${OpenMP_C_TEST_SOURCE}" OpenMP_FLAG_DETECTED)
  set (CMAKE_REQUIRED_FLAGS "${SAFE_CMAKE_REQUIRED_FLAGS}")
  if (OpenMP_FLAG_DETECTED)
    set (OpenMP_C_FLAGS_INTERNAL "${FLAG}")
    break ()
  endif (OpenMP_FLAG_DETECTED)
endforeach (FLAG ${OpenMP_C_FLAG_CANDIDATES})

set (OpenMP_C_FLAGS "${OpenMP_C_FLAGS_INTERNAL}"
  CACHE STRING "C compiler flags for OpenMP parallization")

# handle the standard arguments for find_package
find_package_handle_standard_args (OpenMP DEFAULT_MSG
  OpenMP_C_FLAGS OpenMP_C_FLAGS)

mark_as_advanced (OpenMP_C_FLAGS)
