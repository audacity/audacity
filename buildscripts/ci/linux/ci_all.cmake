
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
    COMMAND cmake ${CONFIGURE_ARGS} -P ${CI_DIR}/../common/ci_configure.cmake 
)

message(STATUS "~~Build~~")

set(BUILD_ARGS

)

execute_process(
    COMMAND cmake ${BUILD_ARGS} -P ${CI_DIR}/ci_build.cmake
)

message(STATUS "~~Package~~")

set(PACK_ARGS
    -DBUILD_VERSION=4.0.0.1234
)

execute_process(
    COMMAND cmake ${PACK_ARGS} -P ${CI_DIR}/package.cmake
)
