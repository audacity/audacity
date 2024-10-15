
message(STATUS "Build")

# Config
set(ARTIFACTS_DIR "build.artifacts")
set(ROOT_DIR ${CMAKE_CURRENT_LIST_DIR}/../../..)

# Options
set(BUILD_NUMBER "" CACHE STRING "Build number")
set(BUILD_MODE "" CACHE STRING "Build mode")
set(BUILD_REVISION "" CACHE STRING "Build revision")
set(BUILD_WIN_PORTABLE "" CACHE STRING "Build portable")

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
if (BUILD_MODE STREQUAL "devel_build")
    set(APP_BUILD_MODE "dev")
elseif(BUILD_MODE STREQUAL "nightly_build")
    set(APP_BUILD_MODE "dev")
elseif(BUILD_MODE STREQUAL "testing_build")
    set(APP_BUILD_MODE "testing")
elseif(BUILD_MODE STREQUAL "stable_build")
    set(APP_BUILD_MODE "release")
endif()

# Build
set(CONFIG
    -DBUILD_TYPE=release_install
    -DBUILD_MODE=${APP_BUILD_MODE}
    -DBUILD_NUMBER=${BUILD_NUMBER}
    -DBUILD_REVISION=${BUILD_REVISION}
)

execute_process(
    COMMAND "${ROOT_DIR}/ci_build.bat" ${CONFIG}
    RESULT_VARIABLE BUILD_RESULT
)

if (BUILD_RESULT GREATER 0)
    message(FATAL_ERROR "Failed build")
endif()

