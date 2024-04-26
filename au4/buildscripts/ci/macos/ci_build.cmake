
message(STATUS "Build MacOS")

# Config
set(ARTIFACTS_DIR "build.artifacts")
set(ROOT_DIR ${CMAKE_CURRENT_LIST_DIR}/../../..)

# Options
set(BUILD_NUMBER "" CACHE STRING "Build number")
set(BUILD_MODE "" CACHE STRING "Build mode")
set(BUILD_REVISION "" CACHE STRING "Build revision")
set(CRASH_REPORT_URL "" CACHE STRING "Crash report url")
set(VST3_SDK_PATH "" CACHE STRING "Vst3 SDK path")

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


set(BUILD_VST ON)
if (NOT VST3_SDK_PATH)
    message(WARNING "not set VST3_SDK_PATH, build VST module disabled")
    set(BUILD_VST OFF)
endif()

# Build
set(CONFIG
    -DBUILD_TYPE=release_install
    -DBUILD_MODE=${APP_BUILD_MODE}
    -DBUILD_NUMBER=${BUILD_NUMBER}
    -DBUILD_REVISION=${BUILD_REVISION}
)

#$CRASH_REPORT_URL
#$BUILD_VST
#$VST3_SDK_PATH

execute_process(
    COMMAND cmake ${CONFIG} -P ${ROOT_DIR}/ci_build.cmake
    RESULT_VARIABLE BUILD_RESULT
)

if (BUILD_RESULT GREATER 0)
    message(FATAL_ERROR "Failed build or install")
else()
    message(STATUS "Success build and install")
endif()
