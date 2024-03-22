#
# This file is part of KDDockWidgets.
#
# SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
# Author: Sergio Martins <sergio.martins@kdab.com>
#
# SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only
#
# Contact KDAB at <info@kdab.com> for commercial licensing options.
#

# Pass the following variables to cmake to control the build:
#
# -DKDDockWidgets_QT6=[true|false]
#  Build against Qt6 rather than Qt5
#  Default=false (Qt5 will be used even if Qt6 is available)
#
# -DKDDockWidgets_STATIC=[true|false]
#  Build static versions of the libraries
#  Default=false
#
# -DKDDockWidgets_TESTS=[true|false]
#  Build the test harness.
#  Currently ignored (except for Python bindings) unless KDDockWidgets_DEVELOPER_MODE=True.
#  Default=false
#
# -DKDDockWidgets_EXAMPLES=[true|false]
#  Build the examples.
#  Default=true
#
# -DKDDockWidgets_DOCS=[true|false]
#  Build the API documentation. Enables the 'docs' build target.
#  Default=false
#
# -DKDDockWidgets_QTQUICK=[true|false]
#  Build for QtQuick instead of QtWidgets.
#  Default=false
#
# -DKDDockWidgets_PYTHON_BINDINGS=[true|false]
#  Build/Generate python bindings.  Always false for Debug builds
#  (If your shiboken or pyside is installed in a non-standard locations
#  try passing the SHIBOKEN_CUSTOM_PREFIX and PYSIDE_CUSTOM_PREFIX variables.)
#  Default=false
#
# -DKDDockWidgets_PYTHON_BINDINGS_INSTALL_PREFIX=[path]
#  Set an alternative install path for Python bindings
#  Default=CMAKE_INSTALL_PREFIX

# ## DO NOT USE IF YOU ARE AN END-USER.  FOR THE DEVELOPERS ONLY!!
## Special CMake Options for Developers
#
# -DKDDockWidgets_DEVELOPER_MODE=[true|false]
#  Configure the build for a developer setup.
#  Enables some features that are not geared towards end-users.
#  Forces the test harness to be built.
#  Default=false
#
# -DKDDockWidgets_WERROR=[true|false]
#  Compile with the -Werror gcc/clang option (always true for developer-mode)
#  Default=false
#
# -DKDDockWidgets_LINTER=[true|false]
#  Build the layout linter.
#  Ignored unless KDDockWidgets_DEVELOPER_MODE=True
#  Default=true
#
# -DKDDockWidgets_FUZZER=[true|false]
#  Build the fuzzer.
#  Ignored unless KDDockWidgets_DEVELOPER_MODE=True
#  Default=true

cmake_minimum_required(VERSION 3.7)

# Allow using a non-KDAB install location.
set(KDAB_INSTALL True CACHE INTERNAL "Install to default KDAB Location")
if(DEFINED CMAKE_INSTALL_PREFIX)
  if(NOT "${CMAKE_INSTALL_PREFIX}" STREQUAL "")
    set(KDAB_INSTALL False CACHE INTERNAL "Install to non-KDAB Location")
  endif()
endif()

if(${CMAKE_VERSION} VERSION_LESS "3.12.0")
  project(KDDockWidgets LANGUAGES CXX)
else()
  project(KDDockWidgets
          DESCRIPTION "An advanced docking system for Qt"
          HOMEPAGE_URL "https://github.com/KDAB/KDDockWidgets"
          LANGUAGES CXX)
endif()

set(${PROJECT_NAME}_VERSION_MAJOR 1)
set(${PROJECT_NAME}_VERSION_MINOR 4)
set(${PROJECT_NAME}_VERSION_PATCH 95)
set(${PROJECT_NAME}_VERSION ${${PROJECT_NAME}_VERSION_MAJOR}.${${PROJECT_NAME}_VERSION_MINOR}.${${PROJECT_NAME}_VERSION_PATCH})
set(PROJECT_VERSION ${${PROJECT_NAME}_VERSION}) #PROJECT_VERSION is needed by some ECM modules
set(${PROJECT_NAME}_SOVERSION "1.5")

include(FeatureSummary)

option(${PROJECT_NAME}_QT6 "Build against Qt 6" OFF)
option(${PROJECT_NAME}_DEVELOPER_MODE "Developer Mode" OFF)
option(${PROJECT_NAME}_PYTHON_BINDINGS "Build python bindings" OFF)
option(${PROJECT_NAME}_QTQUICK "Build for QtQuick instead of QtWidgets" OFF)
option(${PROJECT_NAME}_STATIC "Build statically" OFF)
option(${PROJECT_NAME}_TESTS "Build the tests" OFF)
option(${PROJECT_NAME}_EXAMPLES "Build the examples" ON)
option(${PROJECT_NAME}_DOCS "Build the API documentation" OFF)
option(${PROJECT_NAME}_WERROR "Use -Werror (will be true for developer-mode unconditionally)" OFF)
option(${PROJECT_NAME}_X11EXTRAS "On Linux, link against QtX11Extras so we can detect if the compositor supports transparency. Not applicable to other platforms or Qt6." ON)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake/ECM/modules")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake/KDAB/modules")

# Set a default build type if none was specified
set(default_build_type "Release")
if(EXISTS "${CMAKE_SOURCE_DIR}/.git" OR ${PROJECT_NAME}_DEVELOPER_MODE)
  set(default_build_type "Debug")
endif()
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to ${default_build_type} as none was specified.")
  set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

if(${PROJECT_NAME}_QT6)
  set(Qt_VERSION_MAJOR 6)
  set(QT_MIN_VERSION "6.0.0")
  find_package(Qt6Widgets ${QT_MIN_VERSION} REQUIRED)
  find_package(Qt6Test ${QT_MIN_VERSION} REQUIRED)
  set(${PROJECT_NAME}_LIBRARY_QTID "-qt6")
else()
  set(Qt_VERSION_MAJOR 5)
  set(QT_MIN_VERSION "5.12")
  find_package(Qt5Widgets ${QT_MIN_VERSION} REQUIRED)
  find_package(Qt5Test ${QT_MIN_VERSION} REQUIRED)
  set(${PROJECT_NAME}_LIBRARY_QTID "")
endif()
include(KDQtInstallPaths) #to set QT_INSTALL_FOO variables

set(${PROJECT_NAME}_DEPS "widgets")
if(${PROJECT_NAME}_QTQUICK)
  find_package(Qt${Qt_VERSION_MAJOR}Quick)
  find_package(Qt${Qt_VERSION_MAJOR}QuickControls2)
  add_definitions(-DKDDOCKWIDGETS_QTQUICK)
  set(${PROJECT_NAME}_DEPS "${${PROJECT_NAME}_DEPS} quick quickcontrols2")
else()
  add_definitions(-DKDDOCKWIDGETS_QTWIDGETS)
endif()

if(NOT WIN32 AND NOT APPLE AND NOT EMSCRIPTEN AND NOT ${PROJECT_NAME}_QT6 AND ${PROJECT_NAME}_X11EXTRAS)
  set(${PROJECT_NAME}_DEPS "${${PROJECT_NAME}_DEPS} x11extras")
endif()

#Always build the test harness in developer-mode
if(${PROJECT_NAME}_DEVELOPER_MODE)
  set(${PROJECT_NAME}_TESTS ON)
  set(${PROJECT_NAME}_WERROR ON)
  include(ECMEnableSanitizers)
endif()

if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
  set(IS_CLANG_BUILD TRUE)
else()
  set(IS_CLANG_BUILD FALSE)
endif()

if(${PROJECT_NAME}_QTQUICK)
  set(CMAKE_CXX_STANDARD 17)
  set(CMAKE_CXX_STANDARD_REQUIRED ON)
endif()

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

# Default to hidden visibility for symbols
set(CMAKE_C_VISIBILITY_PRESET hidden)
set(CMAKE_CXX_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN 1)

macro(set_compiler_flags targetName)
  if(${PROJECT_NAME}_DEVELOPER_MODE)
    target_compile_definitions(${targetName} PUBLIC DOCKS_DEVELOPER_MODE PRIVATE QT_FORCE_ASSERTS)

    if(NOT MSVC)
      target_compile_options(${targetName} PRIVATE -Wall -Wextra)
    endif()

    if(APPLE)
      target_compile_options(${targetName} PRIVATE -Wweak-vtables)
    endif()
  endif()

  # Enable -Werror
  if(${PROJECT_NAME}_WERROR AND (NOT MSVC OR IS_CLANG_BUILD)) # clang-cl accepts these too
    target_compile_options(${targetName} PRIVATE -Werror -Wundef -Wno-error=deprecated-declarations)
  endif()
endmacro()

if((CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND NOT APPLE) OR
  (CMAKE_CXX_COMPILER_ID MATCHES "Clang" AND NOT APPLE) OR
  (CMAKE_CXX_COMPILER_ID STREQUAL "Intel" AND NOT WIN32))
  # Linker warnings should be treated as errors
  set(CMAKE_SHARED_LINKER_FLAGS "-Wl,--fatal-warnings ${CMAKE_SHARED_LINKER_FLAGS}")
  set(CMAKE_MODULE_LINKER_FLAGS "-Wl,--fatal-warnings ${CMAKE_MODULE_LINKER_FLAGS}")

  string(TOUPPER "CMAKE_CXX_FLAGS_${CMAKE_BUILD_TYPE}" compileflags)
  if("${CMAKE_CXX_FLAGS} ${${compileflags}}" MATCHES "-fsanitize")
    set(sanitizers_enabled TRUE)
  else()
    set(sanitizers_enabled FALSE)
  endif()

  # cannot enable this for clang + sanitizers
  if(NOT sanitizers_enabled OR NOT CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    # Do not allow undefined symbols, even in non-symbolic shared libraries
    set(CMAKE_SHARED_LINKER_FLAGS "-Wl,--no-undefined ${CMAKE_SHARED_LINKER_FLAGS}")
    set(CMAKE_MODULE_LINKER_FLAGS "-Wl,--no-undefined ${CMAKE_MODULE_LINKER_FLAGS}")
  endif()
endif()

if(${PROJECT_NAME}_STATIC)
  set(${PROJECT_NAME}_LIBRARY_MODE "STATIC")
else()
  set(${PROJECT_NAME}_LIBRARY_MODE "SHARED")
endif()

if(KDAB_INSTALL)
  if(UNIX)
    set(CMAKE_INSTALL_PREFIX "/usr/local/KDAB/${PROJECT_NAME}-${${PROJECT_NAME}_VERSION}" CACHE INTERNAL "Install to default KDAB Location")
  elseif(WIN32)
    set(CMAKE_INSTALL_PREFIX "C:\\KDAB\\${PROJECT_NAME}-$$VERSION" CACHE INTERNAL "Install to default KDAB Location")
  endif()
endif()

# setup default install locations
include(InstallLocation)

if(CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
  set(${PROJECT_NAME}_IS_ROOT_PROJECT TRUE)

  message(STATUS "Building ${PROJECT_NAME} ${${PROJECT_NAME}_VERSION} in ${CMAKE_BUILD_TYPE} mode. Installing to ${CMAKE_INSTALL_PREFIX}")

  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/bin")
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib")
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib")

  install(FILES LICENSE.txt README.md DESTINATION ${INSTALL_DOC_DIR})
  install(DIRECTORY LICENSES DESTINATION ${INSTALL_DOC_DIR})

  # Generate .pri file for qmake users
  #TODO: ECM does not support Qt6 yet
  if(Qt_VERSION_MAJOR EQUAL 5 AND
    CMAKE_VERSION VERSION_GREATER "3.11.99" AND NOT CMAKE_CONFIGURATION_TYPES) # Not working with VS generator or older cmake versions
    include(ECMGeneratePriFile)
    set(PROJECT_VERSION_STRING ${${PROJECT_NAME}_VERSION})
    ecm_generate_pri_file(BASE_NAME KDDockWidgets
      LIB_NAME kddockwidgets
      DEPS ${${PROJECT_NAME}_DEPS}
      FILENAME_VAR pri_filename
      INCLUDE_INSTALL_DIR ${CMAKE_INSTALL_INCLUDEDIR}
    )
    install(FILES ${pri_filename} DESTINATION ${ECM_MKSPECS_INSTALL_DIR})
  endif()
else()
  #Always disable tests, examples, docs when used as a submodule
  set(${PROJECT_NAME}_IS_ROOT_PROJECT FALSE)
  set(${PROJECT_NAME}_TESTS FALSE)
  set(${PROJECT_NAME}_EXAMPLES FALSE)
  set(${PROJECT_NAME}_DOCS FALSE)
endif()

if(${PROJECT_NAME}_TESTS)
  enable_testing()
endif()

add_subdirectory(src)

if(${PROJECT_NAME}_PYTHON_BINDINGS)
  if(CMAKE_BUILD_TYPE MATCHES "^[Dd]eb" OR ${PROJECT_NAME}_STATIC)
    message(FATAL_ERROR "** Python Bindings are disabled in debug or static builds.")
  endif()
endif()
if(${PROJECT_NAME}_PYTHON_BINDINGS)
  add_subdirectory(python)
endif()

if(${PROJECT_NAME}_EXAMPLES)
  if(${PROJECT_NAME}_QTQUICK)
    add_subdirectory(examples/qtquick)
  else()
    add_subdirectory(examples/dockwidgets)
    add_subdirectory(examples/minimal)
    add_subdirectory(examples/minimal-mdi)
    set_compiler_flags(kddockwidgets_example)
    set_compiler_flags(kddockwidgets_minimal_example)
  endif()
endif()

if(${PROJECT_NAME}_TESTS)
  if(${PROJECT_NAME}_DEVELOPER_MODE)
    add_subdirectory(tests)

    # Require Qt5.15.1 or higher to run the tests_launcher tests on Mac
    if(NOT APPLE OR Qt5Widgets_VERSION VERSION_GREATER 5.15.0)
      # tst_docks.exe is pretty big (160 tests), so split it in more runs so we can use threads.
      add_test(NAME tst_docks0 COMMAND tests_launcher 0 5)
      add_test(NAME tst_docks1 COMMAND tests_launcher 1 5)
      add_test(NAME tst_docks2 COMMAND tests_launcher 2 5)
      add_test(NAME tst_docks3 COMMAND tests_launcher 3 5)
      add_test(NAME tst_docks4 COMMAND tests_launcher 4 5)
      add_test(NAME tst_docks5 COMMAND tests_launcher 5 5)
      add_test(NAME tst_docks6 COMMAND tests_launcher 6 5)
      add_test(NAME tst_docks7 COMMAND tests_launcher 7 5)
      add_test(NAME tst_docks8 COMMAND tests_launcher 8 5)
      add_test(NAME tst_docks9 COMMAND tests_launcher 9 5)
      add_test(NAME tst_docks10 COMMAND tests_launcher 10 5)
      add_test(NAME tst_docks11 COMMAND tests_launcher 10 5)
      add_test(NAME tst_docks12 COMMAND tests_launcher 11 5)
      add_test(NAME tst_docks13 COMMAND tests_launcher 12 5)
      add_test(NAME tst_docks14 COMMAND tests_launcher 13 5)
      add_test(NAME tst_docks15 COMMAND tests_launcher 14 5)
      add_test(NAME tst_docks16 COMMAND tests_launcher 15 5)
      add_test(NAME tst_docks17 COMMAND tests_launcher 16 5)
      add_test(NAME tst_docks18 COMMAND tests_launcher 17 5)
      add_test(NAME tst_docks19 COMMAND tests_launcher 18 5)
      add_test(NAME tst_docks20 COMMAND tests_launcher 19 5)
      add_test(NAME tst_docks21 COMMAND tests_launcher 20 5) # one more for rounding leftovers
    endif()
    if(NOT ${PROJECT_NAME}_QTQUICK)
      # tst_multisplitter depends on QWidget
      add_test(NAME tst_multisplitter COMMAND tst_multisplitter)
    endif()

  endif()
endif()

if(${PROJECT_NAME}_DOCS)
  add_subdirectory(docs) # needs to go last, in case there are build source files
endif()

if(${PROJECT_NAME}_IS_ROOT_PROJECT)
   # Add uninstall target (not for submodules since parent projects typically have uninstall too)
  include(ECMUninstallTarget)
endif()

#feature_summary(WHAT ALL INCLUDE_QUIET_PACKAGES FATAL_ON_MISSING_REQUIRED_PACKAGES)
