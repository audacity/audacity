project(taglib)

cmake_minimum_required(VERSION 2.6.0 FATAL_ERROR)

option(ENABLE_STATIC "Make static version of libtag"  OFF)
if(ENABLE_STATIC)
  add_definitions(-DTAGLIB_STATIC)
  set(BUILD_SHARED_LIBS OFF)
else()
  set(BUILD_SHARED_LIBS ON)
endif()
OPTION(ENABLE_STATIC_RUNTIME "Visual Studio, link with runtime statically"  OFF)

option(VISIBILITY_HIDDEN "Build with -fvisibility=hidden"  OFF)
if(VISIBILITY_HIDDEN)
  add_definitions (-fvisibility=hidden)
endif()

option(BUILD_TESTS "Build the test suite"  OFF)
option(BUILD_EXAMPLES "Build the examples"  OFF)

option(NO_ITUNES_HACKS "Disable workarounds for iTunes bugs"  OFF)

add_definitions(-DHAVE_CONFIG_H)
set(TESTS_DIR "${CMAKE_CURRENT_SOURCE_DIR}/tests/")

## the following are directories where stuff will be installed to
set(LIB_SUFFIX "" CACHE STRING "Define suffix of directory name (32/64)")
set(EXEC_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}" CACHE PATH "Base directory for executables and libraries" FORCE)
set(BIN_INSTALL_DIR "${EXEC_INSTALL_PREFIX}/bin" CACHE PATH "The subdirectory to the binaries prefix (default prefix/bin)" FORCE)
set(LIB_INSTALL_DIR "${EXEC_INSTALL_PREFIX}/lib${LIB_SUFFIX}" CACHE PATH "The subdirectory relative to the install prefix where libraries will be installed (default is /lib${LIB_SUFFIX})" FORCE)
set(INCLUDE_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/include" CACHE PATH "The subdirectory to the header prefix" FORCE)

if(APPLE)
	option(BUILD_FRAMEWORK "Build an OS X framework" OFF)
	set(FRAMEWORK_INSTALL_DIR "/Library/Frameworks" CACHE STRING "Directory to install frameworks to.")
endif()

if(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")
endif()
if (MSVC AND ENABLE_STATIC_RUNTIME)
  foreach(flag_var CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO)
    string(REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
  endforeach(flag_var)
endif()

set(TAGLIB_LIB_MAJOR_VERSION "1")
set(TAGLIB_LIB_MINOR_VERSION "9")
set(TAGLIB_LIB_PATCH_VERSION "1")

set(TAGLIB_LIB_VERSION_STRING "${TAGLIB_LIB_MAJOR_VERSION}.${TAGLIB_LIB_MINOR_VERSION}.${TAGLIB_LIB_PATCH_VERSION}")

# 1. If the library source code has changed at all since the last update, then increment revision.
# 2. If any interfaces have been added, removed, or changed since the last update, increment current, and set revision to 0.
# 3. If any interfaces have been added since the last public release, then increment age.
# 4. If any interfaces have been removed since the last public release, then set age to 0. 
set(TAGLIB_SOVERSION_CURRENT  15)
set(TAGLIB_SOVERSION_REVISION 0)
set(TAGLIB_SOVERSION_AGE      14)

math(EXPR TAGLIB_SOVERSION_MAJOR "${TAGLIB_SOVERSION_CURRENT} - ${TAGLIB_SOVERSION_AGE}")
math(EXPR TAGLIB_SOVERSION_MINOR "${TAGLIB_SOVERSION_AGE}")
math(EXPR TAGLIB_SOVERSION_PATCH "${TAGLIB_SOVERSION_REVISION}")

include(ConfigureChecks.cmake)

if(NOT WIN32)
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/taglib-config.cmake ${CMAKE_CURRENT_BINARY_DIR}/taglib-config )
  install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/taglib-config DESTINATION ${BIN_INSTALL_DIR})
endif()

if(WIN32)
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/taglib-config.cmd.cmake ${CMAKE_CURRENT_BINARY_DIR}/taglib-config.cmd )
  install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/taglib-config.cmd DESTINATION ${BIN_INSTALL_DIR})
endif()

if(NOT WIN32 AND NOT BUILD_FRAMEWORK)
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/taglib.pc.cmake ${CMAKE_CURRENT_BINARY_DIR}/taglib.pc )
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/taglib.pc DESTINATION ${LIB_INSTALL_DIR}/pkgconfig)
endif()

include_directories(${CMAKE_CURRENT_BINARY_DIR})
configure_file(config.h.cmake ${CMAKE_CURRENT_BINARY_DIR}/config.h)

if(WITH_ASF)
  set(TAGLIB_WITH_ASF TRUE)
endif()
if(WITH_MP4)
  set(TAGLIB_WITH_MP4 TRUE)
endif()

option(TRACE_IN_RELEASE "Output debug messages even in release mode"  OFF)
if(TRACE_IN_RELEASE)
  set(TRACE_IN_RELEASE TRUE)
endif()

configure_file(taglib/taglib_config.h.cmake ${CMAKE_CURRENT_BINARY_DIR}/taglib_config.h)

add_subdirectory(taglib) 
add_subdirectory(bindings)
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif(BUILD_TESTS)
add_subdirectory(examples)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.cmake ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)
file(COPY doc/taglib.png DESTINATION doc)
add_custom_target(docs doxygen)

# uninstall target
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
  IMMEDIATE @ONLY)

add_custom_target(uninstall
  COMMAND ${CMAKE_COMMAND} -P "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake")
