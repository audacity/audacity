# SoX Resampler Library       Copyright (c) 2007-18 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

cmake_minimum_required (VERSION 3.1 FATAL_ERROR)

project (soxr C)
set (DESCRIPTION_SUMMARY
    "High quality, one-dimensional sample-rate conversion library")



# Release versioning:

set (PROJECT_VERSION_MAJOR 0)
set (PROJECT_VERSION_MINOR 1)
set (PROJECT_VERSION_PATCH 3)

# For shared-object; if, since the last public release:
#   1) library code changed at all: ++revision
#   2) interfaces changed at all:   ++current, revision = 0
#   3) interfaces added:            ++age
#   4) interfaces removed:          age = 0

set (SO_VERSION_CURRENT  1)
set (SO_VERSION_REVISION 2)
set (SO_VERSION_AGE      1)

math (EXPR SO_VERSION_MAJOR "${SO_VERSION_CURRENT} - ${SO_VERSION_AGE}")
math (EXPR SO_VERSION_MINOR "${SO_VERSION_AGE}")
math (EXPR SO_VERSION_PATCH "${SO_VERSION_REVISION}")



# Main options:

include (CMakeDependentOption)

if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE Release CACHE STRING
    "Build type, one of: None Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif ()

option (BUILD_TESTS "Build sanity-tests." ON)
option (BUILD_EXAMPLES "Build examples." OFF)
option (WITH_OPENMP "Include OpenMP threading." ON)
option (WITH_LSR_BINDINGS "Include a `libsamplerate'-like interface." ON)

cmake_dependent_option (BUILD_SHARED_LIBS
  "Build shared (dynamic) soxr libraries." ON
  "NOT WITH_DEV_GPROF" OFF)
cmake_dependent_option (WITH_VR32
  "Include HQ variable-rate resampling engine." ON
  "WITH_CR32 OR WITH_CR64 OR WITH_CR32S OR WITH_CR64S OR NOT DEFINED WITH_VR32" ON)
cmake_dependent_option (WITH_CR32
  "Include HQ constant-rate resampling engine." ON
  "WITH_VR32 OR WITH_CR64 OR WITH_CR32S OR WITH_CR64S" ON)
cmake_dependent_option (WITH_CR64
  "Include VHQ constant-rate resampling engine." ON
  "WITH_VR32 OR WITH_CR32 OR WITH_CR32S OR WITH_CR64S" ON)
cmake_dependent_option (WITH_CR64S
  "Include VHQ SIMD constant-rate resampling engine." ON
  "WITH_VR32 OR WITH_CR32 OR WITH_CR32S OR WITH_CR64" ON)
cmake_dependent_option (WITH_CR32S
  "Include HQ SIMD constant-rate resampling engine." ON
  "WITH_VR32 OR WITH_CR64 OR WITH_CR32 OR WITH_CR64S" ON)
cmake_dependent_option (WITH_PFFFT
  "Use PFFFT (BSD-like licence) for HQ SIMD DFT." ON
  "WITH_CR32S;NOT WITH_AVFFT" OFF)
cmake_dependent_option (WITH_AVFFT
  "Use libavcodec (LGPL) for HQ SIMD DFT." OFF
  "WITH_CR32S;NOT WITH_PFFFT" OFF)
cmake_dependent_option (BUILD_LSR_TESTS "Build LSR tests." OFF
  "UNIX;NOT CMAKE_CROSSCOMPILING;EXISTS ${PROJECT_SOURCE_DIR}/lsr-tests;WITH_LSR_BINDINGS" OFF)

option (WITH_HI_PREC_CLOCK "Enable high-precision time-base." ON)
option (WITH_FLOAT_STD_PREC_CLOCK
  "Use floating-point for standard-precision time-base." OFF)
option (WITH_DEV_TRACE "Enable developer trace capability." ON)
option (WITH_DEV_GPROF "Enable developer grpof output." OFF)
mark_as_advanced (WITH_HI_PREC_CLOCK WITH_FLOAT_STD_PREC_CLOCK
  WITH_DEV_TRACE WITH_DEV_GPROF)



# Introspection:

list (APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/Modules)

include (CheckFunctionExists)
include (CheckIncludeFiles)
include (CheckLibraryExists)
include (SetSystemProcessor)
include (TestBigEndian)

set_system_processor ()

check_library_exists (m pow "" NEED_LIBM)
if (NEED_LIBM)
  set (CMAKE_REQUIRED_LIBRARIES "m;${CMAKE_REQUIRED_LIBRARIES}")
  set (LIBM_LIBRARIES m)
endif ()

if (${BUILD_EXAMPLES})
  project (${PROJECT_NAME}) # Adds c++ compiler
endif ()

if (WITH_OPENMP)
  find_package (OpenMP)
  if (OPENMP_FOUND)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    if (MINGW) # Is this still needed?
      set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_C_FLAGS}")
      set (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${OpenMP_C_FLAGS}")
    endif ()
  endif()
endif ()

if (WITH_CR32S)
  find_package (SIMD32)
  set (WITH_CR32S ${SIMD32_FOUND})
endif ()

if (WITH_CR64S)
  find_package (SIMD64)
  set (WITH_CR64S ${SIMD64_FOUND})
endif ()

if (WITH_AVFFT)
  find_package (LibAVCodec REQUIRED)
  if (AVCODEC_FOUND)
    include_directories (${AVCODEC_INCLUDE_DIRS})
    set (LIBS ${LIBS} ${AVCODEC_LIBRARIES})
  endif ()
endif ()

if (WITH_AVFFT OR (CMAKE_SYSTEM_PROCESSOR MATCHES "^arm" AND SIMD32_FOUND AND WITH_CR32))
  find_package (LibAVUtil)
  if (AVUTIL_FOUND)
    include_directories (${AVUTIL_INCLUDE_DIRS})
    set (LIBS ${LIBS} ${AVUTIL_LIBRARIES})
  endif ()
endif ()

check_function_exists (lrint HAVE_LRINT)
check_include_files (fenv.h HAVE_FENV_H)
check_include_files (stdbool.h HAVE_STDBOOL_H)
check_include_files (stdint.h HAVE_STDINT_H)
test_big_endian (HAVE_BIGENDIAN)



# Compiler configuration:

if (CMAKE_C_COMPILER_ID STREQUAL "GNU" OR CMAKE_C_COMPILER_ID STREQUAL "Clang")
  set (PROJECT_CXX_FLAGS "${PROJECT_CXX_FLAGS} -Wconversion -Wall -Wextra \
      -pedantic -Wundef -Wpointer-arith -Wno-long-long")
  if (CMAKE_C_COMPILER_ID STREQUAL "Clang")
    set (PROJECT_CXX_FLAGS "${PROJECT_CXX_FLAGS} -Wno-keyword-macro")
  endif ()
  if (WITH_DEV_GPROF)
    set (PROJECT_CXX_FLAGS "${PROJECT_CXX_FLAGS} -pg")
  endif ()
  # Can use std=c89, but gnu89 should give faster sinf, cosf, etc.:
  set (PROJECT_C_FLAGS "${PROJECT_CXX_FLAGS} \
       -std=gnu89 -Wnested-externs -Wmissing-prototypes -Wstrict-prototypes")
  if (CMAKE_BUILD_TYPE STREQUAL "Release")
    set (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -s") # strip
  endif ()
  cmake_dependent_option (VISIBILITY_HIDDEN
    "Build shared libraries with -fvisibility=hidden." ON
    "BUILD_SHARED_LIBS" OFF)
  mark_as_advanced (VISIBILITY_HIDDEN)
  if (VISIBILITY_HIDDEN)
    add_definitions (-fvisibility=hidden -DSOXR_VISIBILITY)
  endif ()
endif ()

if (MSVC)
  add_definitions (-D_USE_MATH_DEFINES -D_CRT_SECURE_NO_WARNINGS)
  option (BUILD_SHARED_RUNTIME "MSVC, link with runtime dynamically."  ON)
  if (NOT BUILD_SHARED_RUNTIME)
    foreach (flag_var
        CMAKE_C_FLAGS                CMAKE_CXX_FLAGS
        CMAKE_C_FLAGS_DEBUG          CMAKE_CXX_FLAGS_DEBUG
        CMAKE_C_FLAGS_RELEASE        CMAKE_CXX_FLAGS_RELEASE
        CMAKE_C_FLAGS_MINSIZEREL     CMAKE_CXX_FLAGS_MINSIZEREL
        CMAKE_C_FLAGS_RELWITHDEBINFO CMAKE_CXX_FLAGS_RELWITHDEBINFO)
      string (REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
    endforeach ()
  endif ()
  # By default, do not warn when built on machines using only VS Express:
  if (NOT DEFINED CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS)
    set (CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS ON)
  endif ()
endif ()



# Build configuration:

if (${BUILD_SHARED_LIBS} AND ${CMAKE_SYSTEM_NAME} STREQUAL Windows)
  # Allow exes to find dlls:
  set (BIN ${PROJECT_BINARY_DIR}/bin/)
  set (EXAMPLES_BIN ${BIN})
  set (CMAKE_LIBRARY_OUTPUT_DIRECTORY ${BIN})
  set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${BIN})
else ()
  set (BIN ./)
  set (EXAMPLES_BIN ../examples/)
endif ()

set (LIB_TYPE STATIC)
if (BUILD_SHARED_LIBS)
  set (LIB_TYPE SHARED)
  if (MSVC)
    add_definitions (-DSOXR_DLL)
  endif ()
endif ()

if (CMAKE_BUILD_TYPE STREQUAL "None") # As used by some distros.
  add_definitions (-DNDEBUG)
endif ()



# Installation configuration:

if (NOT DEFINED BIN_INSTALL_DIR)
  set (BIN_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/bin")
endif ()
if (NOT DEFINED LIB_INSTALL_DIR)
  set (LIB_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}")
endif ()
if (NOT DEFINED INCLUDE_INSTALL_DIR)
  set (INCLUDE_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/include")
endif ()
if (NOT DEFINED DOC_INSTALL_DIR)
  if (UNIX)
    set (DOC_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/share/doc/lib${PROJECT_NAME}")
  else ()
    set (DOC_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/doc")
  endif ()
endif ()

if (APPLE)
  option (BUILD_FRAMEWORK "Build an OS X framework." OFF)
  set (FRAMEWORK_INSTALL_DIR
      "/Library/Frameworks" CACHE STRING "Directory to install frameworks to.")
endif ()



# Top-level:

set (PROJECT_VERSION
    ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}.${PROJECT_VERSION_PATCH})
set (SO_VERSION ${SO_VERSION_MAJOR}.${SO_VERSION_MINOR}.${SO_VERSION_PATCH})

configure_file (
  ${PROJECT_SOURCE_DIR}/${PROJECT_NAME}-config.h.in
  ${PROJECT_BINARY_DIR}/${PROJECT_NAME}-config.h)
include_directories (${PROJECT_BINARY_DIR})

if (NOT CMAKE_CROSSCOMPILING AND (BUILD_TESTS OR BUILD_LSR_TESTS))
  enable_testing ()
endif ()

install (FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/README
  ${CMAKE_CURRENT_SOURCE_DIR}/LICENCE
  ${CMAKE_CURRENT_SOURCE_DIR}/NEWS
  DESTINATION ${DOC_INSTALL_DIR})



# Subdirectories:

include_directories (${PROJECT_SOURCE_DIR}/src)

add_subdirectory (src)
if (BUILD_TESTS)
  add_subdirectory (tests)
endif ()
if (BUILD_LSR_TESTS)
  add_subdirectory (lsr-tests)
endif ()
if (BUILD_EXAMPLES OR BUILD_TESTS)
  add_subdirectory (examples)
endif ()



# GNU Autotools compatibility; 'make check':

add_custom_target (check COMMAND ${CMAKE_CTEST_COMMAND})



# GNU Autotools compatibility; 'make distclean':

if (UNIX)
  add_custom_target (distclean COMMAND make clean && find .
      \\! -path \\*/Modules/\\* \\! -name cmp-test.cmake -a -name \\*.cmake
      -o -name CMakeFiles -o -name Makefile -o -name CMakeCache.txt -o -name
      Testing -o -name cmake_install.cmake -o -name install_manifest.txt -o
      -path ./soxr-config.h -o -name config.h -o -name \\*.pc -o -name \\*.s32
      | xargs rm -rf)
endif ()



# Deinstallation:

configure_file (
  "${CMAKE_CURRENT_SOURCE_DIR}/deinstall.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/deinstall.cmake"
  IMMEDIATE @ONLY)

add_custom_target (deinstall
  COMMAND ${CMAKE_COMMAND} -P "${CMAKE_CURRENT_BINARY_DIR}/deinstall.cmake")



# Packaging:

if (UNIX)
  set (CPACK_PACKAGE_VERSION_MAJOR "${PROJECT_VERSION_MAJOR}")
  set (CPACK_PACKAGE_VERSION_MINOR "${PROJECT_VERSION_MINOR}")
  set (CPACK_PACKAGE_VERSION_PATCH "${PROJECT_VERSION_PATCH}")
  set (CPACK_SOURCE_GENERATOR "TXZ")
  set (CPACK_SOURCE_IGNORE_FILES
      "dist;/lsr-tests/;/Debug.*/;/Release.*/;\\\\.swp$;\\\\.git.*;/\\\\.git/")
  include (CPack)
endif ()
