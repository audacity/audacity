# SoX Resampler Library       Copyright (c) 2007-12 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

if (${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
  message(FATAL_ERROR "Usage: cmake <path-to-source>  (from another directory)")
endif ()

cmake_minimum_required (VERSION 2.8)

project (soxr C)
enable_testing ()

set (VERSION_MAJOR 0)
set (VERSION_MINOR 0)
set (VERSION_PATCH 1)

set (VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH})



# Options:

include (CMakeDependentOption)

if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif ()

option (BUILD_SHARED_LIBS "Build shared libraries." ON)
option (WITH_OPENMP "Include OpenMP threading." ON)
option (WITH_LSR_IF "Include a `libsamplerate'-like interface." ON)
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
  cmake_dependent_option (WITH_LSR_TESTS "Build LSR tests." OFF
    "WITH_LSR_IF" OFF)
endif ()



# Introspection:

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/Modules)

include (CheckFunctionExists)
include (CheckIncludeFiles)
include (CheckLibraryExists)
include (TestBigEndian)

check_library_exists(m pow "" NEED_LIBM)
if (NEED_LIBM)
  set(CMAKE_REQUIRED_LIBRARIES "m;${CMAKE_REQUIRED_LIBRARIES}")
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
endif()

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

macro (make_exist)
  foreach (x ${ARGN})
    if (NOT ${x})
      set (${x} 0)
    endif ()
  endforeach ()
endmacro ()

check_function_exists (lrint HAVE_LRINT)
check_include_files(fenv.h HAVE_FENV_H)
test_big_endian(WORDS_BIGENDIAN)
make_exist (HAVE_LRINT HAVE_FENV_H WORDS_BIGENDIAN HAVE_SIMD)
make_exist (HAVE_SINGLE_PRECISION HAVE_DOUBLE_PRECISION HAVE_AVFFT)

configure_file (
  ${PROJECT_SOURCE_DIR}/${PROJECT_NAME}-config.h.in
  ${PROJECT_BINARY_DIR}/${PROJECT_NAME}-config.h)
include_directories (${PROJECT_BINARY_DIR})



# Compiler configuration:

if (CMAKE_COMPILER_IS_GNUCC)
  set (PROJECT_DEFS "-Wconversion -Wall -W -Wmissing-prototypes
  -Wstrict-prototypes -pedantic -Wundef -Wno-long-long")
  if (CMAKE_BUILD_TYPE STREQUAL "Release")
    set (CMAKE_SHARED_LINKER_FLAGS "-s") # strip
  endif ()
endif ()

if (MSVC)
  add_definitions (-D_USE_MATH_DEFINES -D_CRT_SECURE_NO_WARNINGS)
  # By default, do not warn when built on machines using only VS Express:
  if (NOT DEFINED CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS)
    set (CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS ON)
  endif ()
endif ()



# Build configuration:

if (${BUILD_SHARED_LIBS} AND ${CMAKE_SYSTEM_NAME} STREQUAL Windows) # Allow exes to find dlls:
  set (bin ${PROJECT_BINARY_DIR}/bin/)
  set (obin ${bin})
  SET (CMAKE_LIBRARY_OUTPUT_DIRECTORY ${bin}
    CACHE PATH "Single directory for all shared libraries")
  SET (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${bin}
    CACHE PATH "Single directory for all executables.")
else ()
  set (bin ./)
  set (obin ../examples/)
endif ()

set (LIB_TYPE STATIC)
if (BUILD_SHARED_LIBS)
  set (LIB_TYPE SHARED)
  if (MSVC)
    add_definitions (-DSOXR_DLL)
  endif ()
endif ()



# Subdirectories:

include_directories (${PROJECT_SOURCE_DIR}/src)

add_subdirectory (src)
add_subdirectory (examples)
add_subdirectory (tests)
if (WITH_LSR_TESTS)
  add_subdirectory (lsr-tests)
endif ()



# Packaging:

if (UNIX)
  set (CPACK_PACKAGE_VERSION_MAJOR "${VERSION_MAJOR}")
  set (CPACK_PACKAGE_VERSION_MINOR "${VERSION_MINOR}")
  set (CPACK_PACKAGE_VERSION_PATCH "${VERSION_PATCH}")

  set (CPACK_SOURCE_GENERATOR "TBZ2")
  set (CPACK_SOURCE_IGNORE_FILES "/Debug/;/Release/;/cpack/;/dev/;\\\\.swp$")

  include (CPack)

  if (IS_DIRECTORY ${PROJECT_SOURCE_DIR}/cpack)
    add_subdirectory (cpack)
  endif ()
endif ()
