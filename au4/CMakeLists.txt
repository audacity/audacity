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

set(CMAKE_MODULE_PATH
    ${CMAKE_CURRENT_LIST_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/build/cmake
    ${CMAKE_MODULE_PATH}
    )

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

set(AUDACITY_REVISION "" CACHE STRING "Build revision")

# Modules framework (alphabetical order please)
option(MU_BUILD_ACCESSIBILITY_MODULE "Build accessibility module" ON)
option(MU_BUILD_ACCESSIBILITY_TESTS "Build accessibility tests" ON)

# Modules (alphabetical order please)
option(MU_BUILD_APPSHELL_MODULE "Build appshell module" OFF)

# === Setup ===

# === Pack ===
option(MU_RUN_LRELEASE "Generate .qm files" ON)

# === Tests ===
option(MU_BUILD_UNIT_TESTS "Build unit tests" OFF)
option(MU_BUILD_ASAN "Enable Address Sanitizer" OFF)
option(MU_BUILD_CRASHPAD_CLIENT "Build crashpad client" ON)
set(MU_CRASH_REPORT_URL "" CACHE STRING "URL where to send crash reports")
option(MU_CRASHPAD_HANDLER_PATH "Path to custom crashpad_handler executable (optional)" "")

# === Tools ===

# === Compile ===
option(MU_COMPILE_BUILD_MACOS_APPLE_SILICON "Build for Apple Silicon architecture. Only applicable on Macs with Apple Silicon, and requires suitable Qt version." OFF)
option(MU_COMPILE_INSTALL_QTQML_FILES "Whether to bundle qml files along with the installation (relevant on MacOS only)" ON)
option(MU_COMPILE_USE_PCH "Use precompiled headers." ON)
option(MU_COMPILE_USE_UNITY "Use unity build." ON)
option(MU_COMPILE_USE_CCACHE "Try use ccache" ON)

# === Debug ===
option(MU_ENABLE_LOGGER_DEBUGLEVEL "Enable logging debug level" ON)
option(MU_ENABLE_ACCESSIBILITY_TRACE "Enable accessibility logging" OFF)
option(MU_DISABLE_UI_MODALITY "Disable dialogs modality for testing purpose" OFF)
option(MU_ENABLE_STRING_DEBUG_HACK "Enable string debug hack (only clang)" ON)


###########################################
# Setup Configure
###########################################
if(EXISTS "${CMAKE_CURRENT_LIST_DIR}/SetupConfigure.local.cmake")
    include(${CMAKE_CURRENT_LIST_DIR}/SetupConfigure.local.cmake)
else()
    #include(SetupConfigure)
endif()

set(THIRDPARTY_DIR ${PROJECT_SOURCE_DIR}/thirdparty)

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

if (MU_BUILD_UNIT_TESTS)
    enable_testing()
    message(STATUS "Enabled testing")

    define_property(TARGET PROPERTY OUTPUT_XML
        BRIEF_DOCS "List XML files outputted by google test."
        FULL_DOCS "List XML files outputted by google test."
    )

    add_subdirectory(thirdparty/googletest)
endif() #MU_BUILD_UNIT_TESTS

add_subdirectory(src/framework/global) # should be first to work pch
add_subdirectory(src)

###########################################
# Setup the KDDockWidgets lib
###########################################
if (NOT BUILD_SHARED_LIBS)
    set(KDDockWidgets_STATIC ON CACHE BOOL "Build static versions of the libraries" FORCE)
endif()
set(KDDockWidgets_QT6 ON CACHE BOOL "Build against Qt 6" FORCE)
set(KDDockWidgets_QTQUICK ON CACHE BOOL "Build for QtQuick instead of QtWidgets" FORCE)
set(KDDockWidgets_EXAMPLES OFF CACHE BOOL "Build the examples" FORCE)
#add_subdirectory(thirdparty/KDDockWidgets)

###########################################
# Setup Packaging
###########################################
