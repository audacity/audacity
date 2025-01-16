# **********************************************************************
#
#  Audacity: A Digital Audio Editor
#
# **********************************************************************

message(STATUS "Build Linux")

# Config
set(BUILD_TOOLS "$ENV{HOME}/build_tools")
set(ARTIFACTS_DIR "build.artifacts")
set(ROOT_DIR ${CMAKE_CURRENT_LIST_DIR}/../../..)

# Options
set(BUILD_NUMBER "" CACHE STRING "Build number")
set(BUILD_MODE "" CACHE STRING "Build mode")
set(BUILD_REVISION "" CACHE STRING "Build revision")
set(BUILD_ENABLE_CODE_COVERAGE "" CACHE STRING "Build with code coverage")

if (NOT BUILD_NUMBER)
    file (STRINGS "${ARTIFACTS_DIR}/env/build_number.env" BUILD_NUMBER)
endif()

if (NOT BUILD_MODE)
    file (STRINGS "${ARTIFACTS_DIR}/env/build_mode.env" BUILD_MODE)
endif()

if (NOT BUILD_REVISION)
    file (STRINGS "${ARTIFACTS_DIR}/env/build_revision.env" BUILD_REVISION)
endif()

set(APP_BUILD_MODE "dev")
set(APP_SUFFIX "dev")
if (BUILD_MODE STREQUAL "devel_build")
    set(APP_BUILD_MODE "dev")
    set(APP_SUFFIX "dev")
elseif(BUILD_MODE STREQUAL "nightly_build")
    set(APP_BUILD_MODE "dev")
    set(APP_SUFFIX "nightly")
elseif(BUILD_MODE STREQUAL "testing_build")
    set(APP_BUILD_MODE "testing")
    set(APP_SUFFIX "testing")
elseif(BUILD_MODE STREQUAL "stable_build")
    set(APP_BUILD_MODE "release")
    set(APP_SUFFIX "")
endif()

if (BUILD_ENABLE_CODE_COVERAGE STREQUAL "true")
    set(BUILD_ENABLE_CODE_COVERAGE ON)
    set(BUILD_USE_UNITY OFF)
else()
    set(BUILD_ENABLE_CODE_COVERAGE OFF)
    set(BUILD_USE_UNITY OFF) # enable it after fixing the problem with unity
endif()

# Build 
set(CONFIG
    -DBUILD_TYPE=DEBUG_INSTALL
    -DBUILD_CONFIGURATION=UTEST
    -DBUILD_MODE=${APP_BUILD_MODE}
    -DBUILD_NUMBER=${BUILD_NUMBER}
    -DBUILD_REVISION=${BUILD_REVISION}
    -DBUILD_ENABLE_CODE_COVERAGE=${BUILD_ENABLE_CODE_COVERAGE}
    -DBUILD_USE_UNITY=${BUILD_USE_UNITY}
)

execute_process(
    COMMAND cmake ${CONFIG} -P ${ROOT_DIR}/ci_build.cmake
    RESULT_VARIABLE BUILD_RESULT
)

if (BUILD_RESULT GREATER 0) 
    message(FATAL_ERROR "Failed build")
endif()

