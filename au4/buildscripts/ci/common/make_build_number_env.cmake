
# Config
set(ARTIFACTS_DIR "build.artifacts")

# Options
set(BUILD_NUMBER "" CACHE STRING "Build number")

if (NOT BUILD_NUMBER)
    # less than 2147483647 to fit in Int32 for WiX packaging tool
    string(TIMESTAMP BUILD_NUMBER %y%j%H%M)
endif()

file(MAKE_DIRECTORY ${ARTIFACTS_DIR})
file(MAKE_DIRECTORY ${ARTIFACTS_DIR}/env)

file(WRITE ${ARTIFACTS_DIR}/env/build_number.env ${BUILD_NUMBER})
message(STATUS "BUILD_NUMBER: ${BUILD_NUMBER} (${ARTIFACTS_DIR}/env/build_number.env)")

