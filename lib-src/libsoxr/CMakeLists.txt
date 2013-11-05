# SoX Resampler Library       Copyright (c) 2007-13 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

cmake_minimum_required (VERSION 2.8 FATAL_ERROR)

project (soxr C)
set (DESCRIPTION_SUMMARY "High quality, one-dimensional sample-rate conversion library")



# Release versioning:

set (PROJECT_VERSION_MAJOR 0)
set (PROJECT_VERSION_MINOR 1)
set (PROJECT_VERSION_PATCH 1)

# For shared-object; if, since the last public release:
#  * library code changed at all: ++revision
#  * interfaces changed at all:   ++current, revision = 0
#  * interfaces added:            ++age
#  * interfaces removed:          age = 0

set (SO_VERSION_CURRENT  1)
set (SO_VERSION_REVISION 0)
set (SO_VERSION_AGE      1)



# Main options:

include (CMakeDependentOption)

if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif ()

option (BUILD_TESTS "Build sanity-tests."  ON)
option (BUILD_SHARED_LIBS "Build shared libraries." ON)
option (BUILD_EXAMPLES "Build examples." OFF)
option (WITH_OPENMP "Include OpenMP threading." ON)
option (WITH_LSR_BINDINGS "Include a `libsamplerate'-like interface." ON)
cmake_dependent_option (WITH_SINGLE_PRECISION "Build with single precision (for up to 20-bit accuracy)." ON
  "WITH_DOUBLE_PRECISION" ON)
cmake_dependent_option (WITH_DOUBLE_PRECISION "Build with double precision (for up to 32-bit accuracy)." ON
  "WITH_SINGLE_PRECISION" ON)
cmake_dependent_option (WITH_SIMD "Use SIMD (for faster single precision)." ON
  "WITH_SINGLE_PRECISION" OFF)
cmake_dependent_option (WITH_AVFFT "Use libavcodec (LGPL) for SIMD DFT." OFF
  "WITH_SIMD;NOT WITH_PFFFT" OFF)
cmake_dependent_option (WITH_PFFFT "Use PFFFT (BSD-like licence) for SIMD DFT." ON
  "WITH_SIMD;NOT WITH_AVFFT" OFF)
if (UNIX)
  if (EXISTS ${PROJECT_SOURCE_DIR}/lsr-tests)
    cmake_dependent_option (BUILD_LSR_TESTS "Build LSR tests." OFF
      "WITH_LSR_BINDINGS" OFF)
  endif ()
endif ()



# Introspection:

list (APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/Modules)

include (CheckFunctionExists)
include (CheckIncludeFiles)
include (CheckLibraryExists)
include (TestBigEndian)

check_library_exists (m pow "" NEED_LIBM)
if (NEED_LIBM)
  set (CMAKE_REQUIRED_LIBRARIES "m;${CMAKE_REQUIRED_LIBRARIES}")
  link_libraries (m)
endif ()

if (WITH_OPENMP)
  find_package (OpenMP)
endif ()
if (OPENMP_FOUND)
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
endif ()

if (WITH_SIMD)
  find_package (SIMD)
  if (SIMD_FOUND)
    set (HAVE_SIMD 1)
  endif ()
endif ()

if (WITH_SINGLE_PRECISION)
  set (HAVE_SINGLE_PRECISION 1)
endif ()

if (WITH_DOUBLE_PRECISION)
  set (HAVE_DOUBLE_PRECISION 1)
endif ()

if (WITH_AVFFT)
  find_package (LibAVCodec)
  if (AVCODEC_FOUND)
    include_directories (${AVCODEC_INCLUDE_DIRS})
    link_libraries (${AVCODEC_LIBRARIES})
    set (HAVE_AVFFT 1)
  endif ()
endif ()

check_function_exists (lrint HAVE_LRINT)
check_include_files (fenv.h HAVE_FENV_H)
test_big_endian (WORDS_BIGENDIAN)

macro (make_exist)
  foreach (x ${ARGN})
    if (NOT ${x})
      set (${x} 0)
    endif ()
  endforeach ()
endmacro ()

make_exist (HAVE_LRINT HAVE_FENV_H WORDS_BIGENDIAN HAVE_SIMD)
make_exist (HAVE_SINGLE_PRECISION HAVE_DOUBLE_PRECISION HAVE_AVFFT)



# Compiler configuration:

if (CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
  set (PROJECT_CXX_FLAGS "-Wconversion -Wall -W -pedantic -Wundef -Wcast-align -Wpointer-arith -Wno-long-long")
  set (PROJECT_C_FLAGS "${PROJECT_CXX_FLAGS} -Wnested-externs -Wmissing-prototypes -Wstrict-prototypes")
  if (CMAKE_BUILD_TYPE STREQUAL "Release")
    set (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -s") # strip
  endif ()
  cmake_dependent_option (VISIBILITY_HIDDEN "Build with -fvisibility=hidden." ON
    "BUILD_SHARED_LIBS" OFF)
  if (VISIBILITY_HIDDEN)
    add_definitions (-fvisibility=hidden -DSOXR_VISIBILITY)
  endif ()
endif ()

if (MSVC)
  add_definitions (-D_USE_MATH_DEFINES -D_CRT_SECURE_NO_WARNINGS)
  option (ENABLE_STATIC_RUNTIME "Visual Studio, link with runtime statically."  OFF)
  if (ENABLE_STATIC_RUNTIME)
    foreach (flag_var CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO)
      string (REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
    endforeach ()
  endif ()
  # By default, do not warn when built on machines using only VS Express:
  if (NOT DEFINED CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS)
    set (CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS ON)
  endif ()
endif ()



# Build configuration:

if (${BUILD_SHARED_LIBS} AND ${CMAKE_SYSTEM_NAME} STREQUAL Windows) # Allow exes to find dlls:
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
  set (FRAMEWORK_INSTALL_DIR "/Library/Frameworks" CACHE STRING "Directory to install frameworks to.")
endif ()



# Top-level:

set (PROJECT_VERSION ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}.${PROJECT_VERSION_PATCH})
math (EXPR SO_VERSION_MAJOR "${SO_VERSION_CURRENT} - ${SO_VERSION_AGE}")
math (EXPR SO_VERSION_MINOR "${SO_VERSION_AGE}")
math (EXPR SO_VERSION_PATCH "${SO_VERSION_REVISION}")
set (SO_VERSION ${SO_VERSION_MAJOR}.${SO_VERSION_MINOR}.${SO_VERSION_PATCH})

configure_file (
  ${PROJECT_SOURCE_DIR}/${PROJECT_NAME}-config.h.in
  ${PROJECT_BINARY_DIR}/${PROJECT_NAME}-config.h)
include_directories (${PROJECT_BINARY_DIR})

if (BUILD_TESTS OR BUILD_LSR_TESTS)
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



# Rough-and-ready distclean for anyone still doing in-tree builds:

if (UNIX)
  add_custom_target (distclean
    COMMAND make clean && rm -rf
      CMakeCache.txt
      CMakeFiles
      cmake_install.cmake
      CPackConfig.cmake
      CPackSourceConfig.cmake
      deinstall.cmake
      Makefile
      soxr-config.h
      src/CMakeFiles
      src/cmake_install.cmake
      src/libsoxr-dev.src
      src/libsoxr-lsr.pc
      src/libsoxr.pc
      src/libsoxr.src
      src/Makefile)
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

  set (CPACK_SOURCE_GENERATOR "TGZ")
  set (CPACK_SOURCE_IGNORE_FILES "dist;/lsr-tests/;/Debug/;/Release/;/cpack/;\\\\.swp$;\\\\.gitignore;/\\\\.git/")

  include (CPack)

  if (IS_DIRECTORY ${PROJECT_SOURCE_DIR}/cpack)
    add_subdirectory (cpack)
  endif ()
endif ()
