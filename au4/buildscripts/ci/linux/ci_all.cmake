
# Just for local tests
set(CI_DIR ${CMAKE_CURRENT_LIST_DIR})

# Options
set(EVENT "pull_request" CACHE STRING "Action event")
set(BUILD_MODE "" CACHE STRING "Build mode")
set(ARTIFACT_INFO "Make Linux CI" CACHE STRING "Artifact info")

message(STATUS "~~Configure workflow~~")

set(CONFIGURE_ARGS
    -DEVENT=${EVENT}
    -DBUILD_MODE=${BUILD_MODE}
    -DARTIFACT_INFO="Lin_${ARTIFACT_INFO}"
)

execute_process(
    COMMAND cmake ${CONFIGURE_ARGS} -P ${CI_DIR}/configure.cmake
)

message(STATUS "~~Build~~")

execute_process(
    COMMAND cmake -P ${CI_DIR}/build.cmake
)
