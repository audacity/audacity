
message(STATUS "Build Linux")

# Config
set(BUILD_TOOLS "$ENV{HOME}/build_tools")
set(ARTIFACTS_DIR "build.artifacts")
set(ROOT_DIR ${CMAKE_CURRENT_LIST_DIR}/../../..)

# Options
set(BUILD_NUMBER "" CACHE STRING "Build number")
set(BUILD_MODE "" CACHE STRING "Build mode")
set(BUILD_REVISION "" CACHE STRING "Build revision")

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


# Build 
set(CONFIG
    -DBUILD_TYPE=appimage
    -DBUILD_MODE=${APP_BUILD_MODE}
    -DBUILD_NUMBER=${BUILD_NUMBER}
    -DBUILD_REVISION=${BUILD_REVISION}
    -DINSTALL_SUFFIX=${APP_SUFFIX}
)

execute_process(
    COMMAND cmake ${CONFIG} -P ${ROOT_DIR}/ci_build.cmake
    RESULT_VARIABLE BUILD_RESULT
)

if (BUILD_RESULT GREATER 0) 
    message(FATAL_ERROR "Failed build")
endif()

