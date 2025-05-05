# Config
set(ARTIFACTS_DIR "build.artifacts")
set(ROOT_DIR ${CMAKE_CURRENT_LIST_DIR}/../../..)

# Options
set(BUILD_NUMBER "" CACHE STRING "Build number")

if (NOT BUILD_NUMBER)
    file(STRINGS "${ARTIFACTS_DIR}/env/build_number.env" BUILD_NUMBER)
endif()

if (NOT BUILD_NUMBER)
    message(FATAL_ERROR "Not set BUILD_NUMBER")
endif()

file(STRINGS "${ARTIFACTS_DIR}/env/build_revision.env" REVISION)

if (NOT REVISION)
    message(FATAL_ERROR "REVISION not set")
endif()

include(${ROOT_DIR}/version.cmake)

set(BUILD_VERSION "${MUSE_APP_VERSION}.${BUILD_NUMBER}[${REVISION}]")

file(MAKE_DIRECTORY ${ARTIFACTS_DIR})
file(MAKE_DIRECTORY ${ARTIFACTS_DIR}/env)

file(WRITE ${ARTIFACTS_DIR}/env/build_version.env ${BUILD_VERSION})
message(STATUS "BUILD_VERSION: ${BUILD_VERSION} (${ARTIFACTS_DIR}/env/build_version.env)")
