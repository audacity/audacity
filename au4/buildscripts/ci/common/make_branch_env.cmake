
# Config
set(ARTIFACTS_DIR "build.artifacts")

# Options
set(BRANCH "" CACHE STRING "Branch")

if (NOT BRANCH)
    execute_process(
        COMMAND git rev-parse --abbrev-ref HEAD
        OUTPUT_VARIABLE BRANCH
    )
    string(STRIP ${BRANCH} BRANCH)
endif()

if (NOT BRANCH)
    message(WARNING "Not set or unable get branch")
endif()

file(MAKE_DIRECTORY ${ARTIFACTS_DIR})
file(MAKE_DIRECTORY ${ARTIFACTS_DIR}/env)

file(WRITE ${ARTIFACTS_DIR}/env/build_branch.env ${BRANCH})
message(STATUS "BRANCH: ${BRANCH} (${ARTIFACTS_DIR}/env/build_branch.env)")
