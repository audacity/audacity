
# Config
set(ARTIFACTS_DIR "build.artifacts")

set(EVENT "" CACHE STRING "Action event")
set(BUILD_MODE "" CACHE STRING "Build mode")

if (NOT BUILD_MODE)
    if (NOT EVENT)
        message(FATAL_ERROR "not set BUILD_MODE or EVENT")
    endif()

    if (EVENT STREQUAL "pull_request")
        set(BUILD_MODE "devel_build")
    elseif(EVENT STREQUAL "schedule")
        set(BUILD_MODE "nightly_build")
    elseif(EVENT STREQUAL "workflow_dispatch")
        message(FATAL_ERROR "event workflow_dispatch, but not set BUILD_MODE")
    endif()
endif()

# Support new IN_LIST if() operator.
cmake_policy(SET CMP0057 NEW)

set(MODE_IS_VALID FALSE)
set(VALID_MODES "devel_build" "nightly_build" "testing_build" "stable_build")
if (${BUILD_MODE} IN_LIST VALID_MODES)
    set(MODE_IS_VALID TRUE)
endif()

if (NOT MODE_IS_VALID)
    message(FATAL_ERROR "Not valid build mode: ${BUILD_MODE}")
endif()

file(MAKE_DIRECTORY ${ARTIFACTS_DIR})
file(MAKE_DIRECTORY ${ARTIFACTS_DIR}/env)

file(WRITE ${ARTIFACTS_DIR}/env/build_mode.env ${BUILD_MODE})
message(STATUS "BUILD_MODE: ${BUILD_MODE} (${ARTIFACTS_DIR}/env/build_mode.env)")


