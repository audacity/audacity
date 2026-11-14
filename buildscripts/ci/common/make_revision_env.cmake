
# Config
set(ARTIFACTS_DIR "build.artifacts")

# Options
set(REVISION "" CACHE STRING "Revision")

if (NOT REVISION)
    execute_process(
        COMMAND git rev-parse --short=7 HEAD
        OUTPUT_VARIABLE REVISION
    )
    string(STRIP ${REVISION} REVISION)
endif()

if (NOT REVISION)
    message(WARNING "Not set or unable get revision")
endif()

file(MAKE_DIRECTORY ${ARTIFACTS_DIR})
file(MAKE_DIRECTORY ${ARTIFACTS_DIR}/env)

file(WRITE ${ARTIFACTS_DIR}/env/build_revision.env ${REVISION})
message(STATUS "REVISION: ${REVISION} (${ARTIFACTS_DIR}/env/build_revision.env)")

