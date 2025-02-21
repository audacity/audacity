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
  GIT_REPOSITORY https://github.com/musescore/framework_tmp.git
  GIT_TAG        u250221_2
)

FetchContent_GetProperties(muse_framework)
if(NOT muse_framework_POPULATED)
  FetchContent_Populate(muse_framework)
endif()

set(MUSE_FRAMEWORK_PATH ${muse_framework_SOURCE_DIR})
set(MUSE_FRAMEWORK_SRC_PATH ${muse_framework_SOURCE_DIR}/framework)

set(CMAKE_MODULE_PATH
    ${CMAKE_CURRENT_LIST_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/buildscripts/cmake
    ${MUSE_FRAMEWORK_PATH}/buildscripts/cmake
    ${CMAKE_MODULE_PATH}
    )

include(${MUSE_FRAMEWORK_SRC_PATH}/cmake/MuseDeclareOptions.cmake)

###########################################
# AudacityDependencies
###########################################
set(AUDACITY_ROOT ${CMAKE_CURRENT_LIST_DIR}/au3)
add_definitions(-DINSTALL_PREFIX="${CMAKE_INSTALL_PREFIX}")

include(GetPlatformInfo)

include(SetupDependencies)

###########################################
# Setup option and build settings
###########################################
include(GetPaths)

set(AU4_BUILD_CONFIGURATION "app" CACHE STRING "Build configuration")
# Possible values:
# - app             - for desktop app
# - app-portable    - for desktop portable app (Windows build for PortableApps.com)
# - utest           - for unit tests (for CI)

set(AU4_BUILD_MODE "dev" CACHE STRING "Build mode")
# Possible values:
# - dev     - for development/nightly builds
# - testing - for testing versions (alpha, beta, RC)
# - release - for stable release builds

set(AU4_REVISION "" CACHE STRING "Build revision")

set(MUSE_ENABLE_UNIT_TESTS ON)
set(MUSE_MODULE_ACCESSIBILITY ON)
set(MUSE_MODULE_ACTIONS ON)
set(MUSE_MODULE_AUDIO OFF)
set(MUSE_MODULE_AUDIOPLUGINS ON)
set(MUSE_MODULE_AUTOBOT OFF)
set(MUSE_MODULE_CLOUD ON)
set(MUSE_MODULE_CLOUD_MUSESCORECOM OFF CACHE BOOL "Enable MuseScore.com account" FORCE)
set(MUSE_MODULE_DIAGNOSTICS ON)
set(MUSE_MODULE_DRAW ON)
set(MUSE_MODULE_DRAW_TESTS OFF)
set(MUSE_MODULE_EXTENSIONS OFF)
set(MUSE_MODULE_GLOBAL ON)
set(MUSE_MODULE_GLOBAL_TESTS OFF)
set(MUSE_MODULE_LANGUAGES ON)
set(MUSE_MODULE_LEARN ON)
set(MUSE_MODULE_MIDI OFF)
set(MUSE_MODULE_MPE OFF)
set(MUSE_MODULE_MULTIINSTANCES OFF)
set(MUSE_MODULE_MUSESAMPLER OFF)
set(MUSE_MODULE_NETWORK ON)
set(MUSE_MODULE_SHORTCUTS ON)
set(MUSE_MODULE_UI ON)
set(MUSE_MODULE_UI_TESTS OFF)
set(MUSE_MODULE_UPDATE OFF)
set(MUSE_MODULE_WORKSPACE OFF)

# Modules (alphabetical order please)
option(AU_BUILD_APPSHELL_MODULE "Build appshell module" ON)
option(AU_BUILD_CONTEXT_TESTS "Build context tests" ON)
option(AU_BUILD_EFFECTS_MODULE "Build effects module" ON)
option(AU_BUILD_PLAYBACK_MODULE "Build playback module" ON)
option(AU_BUILD_PLAYBACK_TESTS "Build playback tests" ON)
option(AU_BUILD_PROJECT_TESTS "Build project tests" ON)
option(AU_BUILD_PROJECTSCENE_MODULE "Build projectscene modules" ON)
option(AU_BUILD_PROJECTSCENE_TESTS "Build projectscene tests" ON)
option(AU_BUILD_RECORD_MODULE "Build record module" ON)
option(AU_BUILD_RECORD_TESTS "Build record tests" ON)
option(AU_BUILD_TRACKEDIT_TESTS "Build trackedit tests" ON)
option(AU_BUILD_EFFECTS_TESTS "Build effects tests" ON)

option(AU_MODULE_EFFECTS_VST "Build audacity vst module" ON)
set(AU_MODULE_VST_VST3_SDK_PATH "" CACHE PATH "[optional] Path to VST3_SDK. SDK version >= ${VST3_SDK_VERSION} required")

option(AU_USE_FFMPEG "Build mod-ffmpeg module" ON)
if (AU_USE_FFMPEG)
    add_definitions(-DAU_USE_FFMPEG)
endif()

# === Setup ===

# === Pack ===
option(MU_RUN_LRELEASE "Generate .qm files" ON)

# === Compile ===
option(MU_COMPILE_BUILD_MACOS_APPLE_SILICON "Build for Apple Silicon architecture. Only applicable on Macs with Apple Silicon, and requires suitable Qt version." OFF)
option(MU_COMPILE_INSTALL_QTQML_FILES "Whether to bundle qml files along with the installation (relevant on MacOS only)" ON)
option(MUSE_COMPILE_USE_UNITY "Use unity build." ON)
option(MUSE_COMPILE_USE_CCACHE "Try use ccache" ON)


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

###########################################
# Add source tree
###########################################
if (MUSE_ENABLE_UNIT_TESTS)
    enable_testing()
    message(STATUS "Enabled testing")
endif()

add_subdirectory(${muse_framework_SOURCE_DIR}/framework ${muse_framework_BINARY_DIR})
add_subdirectory(src)

if (MSVC)
    # Now that the `audacity` target is defined, we can set it as the startup project in Visual Studio.
    set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT audacity)
endif()

###########################################
# Setup Packaging
###########################################
if (OS_IS_LIN)
    include(${CMAKE_CURRENT_LIST_DIR}/buildscripts/packaging/Linux+BSD/SetupAppImagePackaging.cmake)
endif(OS_IS_LIN)
