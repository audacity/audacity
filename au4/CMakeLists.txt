# **********************************************************************
#
#  Audacity: A Digital Audio Editor
#
# **********************************************************************

cmake_minimum_required(VERSION 3.24)

cmake_policy(SET CMP0091 OLD) # not set MSVC default args

project(audacity LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

###########################################
# Muse Framework
###########################################
set(FETCHCONTENT_QUIET OFF)
set(FETCHCONTENT_BASE_DIR ${CMAKE_CURRENT_LIST_DIR}/_deps)
include(FetchContent)
FetchContent_Declare(
  muse_framework
  GIT_REPOSITORY git@github.com:musescore/framework_tmp.git
  GIT_TAG        HEAD
)

FetchContent_GetProperties(muse_framework)
if(NOT muse_framework_POPULATED)
  FetchContent_Populate(muse_framework)
endif()

set(MUSE_FRAMEWORK_PATH ${muse_framework_SOURCE_DIR})
set(MUSE_FRAMEWORK_SRC_PATH ${muse_framework_SOURCE_DIR}/framework)

set(CMAKE_MODULE_PATH
    ${CMAKE_CURRENT_LIST_DIR}
    ${MUSE_FRAMEWORK_PATH}/build/cmake
    ${CMAKE_MODULE_PATH}
    )

include(${MUSE_FRAMEWORK_SRC_PATH}/cmake/DeclareMuseOptions.cmake)

###########################################
# AudacityDependencies
###########################################
set(AUDACITY_ROOT ${CMAKE_CURRENT_LIST_DIR}/..)
set(AUDACITY_CMAKE_MODULE_PATH "${AUDACITY_ROOT}/cmake-proxies/cmake-modules")
set(CMAKE_MODULE_PATH
    ${CMAKE_MODULE_PATH}
    ${AUDACITY_CMAKE_MODULE_PATH}
    )

set( AUDACITY_VERSION 4 )
set( AUDACITY_RELEASE 0 )
set( AUDACITY_REVISION 0 )
set( AUDACITY_MODLEVEL 0 )

include(AudacityFunctions)
include(CMakeDependentOption)

# Define option() prefix
set( _OPT "audacity_" )
cmd_option(${_OPT}conan_enabled "Use Conan package manager for 3d party dependencies" On)
cmd_option(${_OPT}use_zlib "" "local")
cmd_option(${_OPT}use_png "" "local")
cmd_option(${_OPT}use_jpeg "" "local")
cmd_option(${_OPT}use_expat "" "local")
cmd_option(${_OPT}use_wxwidgets "" "local")
cmd_option(${_OPT}use_libmp3lame "" "local")
cmd_option(${_OPT}use_libmpg123 "" "local")
cmd_option(${_OPT}use_libid3tag "" "local")
cmd_option(${_OPT}use_wavpack "" "local")
cmd_option(${_OPT}use_libogg "" "local")
cmd_option(${_OPT}use_libflac "" "local")
cmd_option(${_OPT}use_libopus "" "local")
cmd_option(${_OPT}use_opusfile "" "local")
cmd_option(${_OPT}use_libvorbis "" "local")
cmd_option(${_OPT}use_libsndfile "" "local")
cmd_option(${_OPT}use_portaudio "" "local")
cmd_option(${_OPT}use_rapidjson "" "local")
cmd_option(${_OPT}use_midi "" "local")

find_package(Python3)
if( Python3_FOUND )
   set(PYTHON "${Python3_EXECUTABLE}")
endif()

set( CMAKE_PREFIX_PATH
   "${CMAKE_BINARY_DIR}/generators"
   ${CMAKE_PREFIX_PATH}
)

set(CONAN_RUNNER_DIR ${AUDACITY_ROOT}/conan)
include(AudacityDependencies)
include(dependencies/wxwidgets)
include(dependencies/expat)

include(AudacityTesting)

# Generate config file
if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   configure_file( ${AUDACITY_ROOT}/src/audacity_config.h.in src/private/configwin.h )
elseif( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   set( HAVE_VISIBILITY 0 )
   configure_file( ${AUDACITY_ROOT}/src/audacity_config.h.in src/private/configmac.h )
else()
   set( HAVE_VISIBILITY 0 )
   configure_file( ${AUDACITY_ROOT}/src/audacity_config.h.in src/private/configunix.h )
endif()

add_definitions(-DINSTALL_PREFIX="${CMAKE_INSTALL_PREFIX}")

###########################################
# Setup option and build settings
###########################################
include(GetPaths)

set(AUDACITY_BUILD_CONFIGURATION "app" CACHE STRING "Build configuration")
# Possible values:
# - app             - for desktop app
# - app-portable    - for desktop portable app (Windows build for PortableApps.com)
# - utest           - for unit tests (for CI)

set(AUDACITY_BUILD_MODE "dev" CACHE STRING "Build mode")
# Possible values:
# - dev     - for development/nightly builds
# - testing - for testing versions (alpha, beta, RC)
# - release - for stable release builds

set(AU_REVISION "" CACHE STRING "Build revision")

set(MUSE_BUILD_UNIT_TESTS OFF)
set(MUSE_MODULE_ACCESSIBILITY OFF)
set(MUSE_MODULE_ACTIONS ON)
set(MUSE_MODULE_AUDIO OFF)
set(MUSE_MODULE_AUTOBOT OFF)
set(MUSE_MODULE_CLOUD ON)
set(MUSE_MODULE_DIAGNOSTICS OFF)
set(MUSE_MODULE_DRAW OFF)
set(MUSE_MODULE_EXTENSIONS OFF)
set(MUSE_MODULE_GLOBAL ON)
set(MUSE_MODULE_LANGUAGES OFF)
set(MUSE_MODULE_LEARN ON)
set(MUSE_MODULE_MIDI OFF)
set(MUSE_MODULE_MPE OFF)
set(MUSE_MODULE_MULTIINSTANCES OFF)
set(MUSE_MODULE_MUSESAMPLER OFF)
set(MUSE_MODULE_NETWORK ON)
set(MUSE_MODULE_SHORTCUTS OFF)
set(MUSE_MODULE_UI ON)
set(MUSE_MODULE_UPDATE OFF)
set(MUSE_MODULE_VST OFF)
set(MUSE_MODULE_WORKSPACE OFF)

# Modules (alphabetical order please)
option(MU_BUILD_APPSHELL_MODULE "Build appshell module" ON)
option(MU_BUILD_PROJECTSCENE_MODULE "Build projectscene modules" ON)
option(MU_BUILD_PROJECTSCENE_TESTS "Build projectscene tests" ON)

# === Setup ===

# === Pack ===
option(MU_RUN_LRELEASE "Generate .qm files" ON)

# === Tests ===
option(MU_BUILD_ASAN "Enable Address Sanitizer" OFF)

# === Tools ===

# === Compile ===
option(MU_COMPILE_BUILD_MACOS_APPLE_SILICON "Build for Apple Silicon architecture. Only applicable on Macs with Apple Silicon, and requires suitable Qt version." OFF)
option(MU_COMPILE_INSTALL_QTQML_FILES "Whether to bundle qml files along with the installation (relevant on MacOS only)" ON)
option(MU_COMPILE_USE_PCH "Use precompiled headers." ON)
option(MU_COMPILE_USE_UNITY "Use unity build." ON)
option(MU_COMPILE_USE_CCACHE "Try use ccache" ON)

# === Debug ===
option(MU_ENABLE_STRING_DEBUG_HACK "Enable string debug hack (only clang)" ON)


###########################################
# Setup Configure
###########################################
if(EXISTS "${CMAKE_CURRENT_LIST_DIR}/SetupConfigure.local.cmake")
    include(${CMAKE_CURRENT_LIST_DIR}/SetupConfigure.local.cmake)
else()
    include(SetupConfigure)
endif()

###########################################
# Setup compiler and build environment
###########################################

include(SetupBuildEnvironment)
include(GetPlatformInfo)
if (MUE_COMPILE_USE_CCACHE)
    include(TryUseCcache)
endif(MUE_COMPILE_USE_CCACHE)


###########################################
# Setup external dependencies
###########################################
set(QT_MIN_VERSION "6.2.4")
include(FindQt6)

if (OS_IS_WIN)
    include(FetchContent)
    FetchContent_Declare(
      muse_prebuild_win_deps
      GIT_REPOSITORY https://github.com/musescore/musescore_prebuild_win_deps.git
      GIT_TAG        HEAD
    )
    FetchContent_MakeAvailable(muse_prebuild_win_deps)
    set(DEPENDENCIES_DIR ${muse_prebuild_win_deps_SOURCE_DIR})
    set(DEPENDENCIES_LIB_DIR ${DEPENDENCIES_DIR}/libx64)
    set(DEPENDENCIES_INC ${DEPENDENCIES_DIR}/include)
endif(OS_IS_WIN)

###########################################
# Add source tree
###########################################

add_subdirectory(${muse_framework_SOURCE_DIR}/framework ${muse_framework_BINARY_DIR})
add_subdirectory(src)

###########################################
# Setup Packaging
###########################################
