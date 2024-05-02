
# Config
set(ARTIFACTS_DIR "build.artifacts")
set(CI_TOOLS ${CMAKE_CURRENT_LIST_DIR})

# Options
set(EVENT "" CACHE STRING "Action event")
set(BUILD_MODE "" CACHE STRING "Build mode")
set(ARTIFACT_INFO "" CACHE STRING "Artifact info")

# Make build mode env
execute_process(
    COMMAND cmake -DEVENT=${EVENT} -DBUILD_MODE=${BUILD_MODE} -P ${CI_TOOLS}/make_build_mode_env.cmake
)

# Make build number env
execute_process(
    COMMAND cmake -P ${CI_TOOLS}/make_build_number_env.cmake
)

# Make version env
execute_process(
    COMMAND cmake -P ${CI_TOOLS}/make_version_env.cmake
)

# Make revision env
execute_process(
    COMMAND cmake -P ${CI_TOOLS}/make_revision_env.cmake
)

# Make revision env
execute_process(
    COMMAND cmake -P ${CI_TOOLS}/make_branch_env.cmake
)

# Make artifact name env
execute_process(
    COMMAND cmake -DPREFIX=AU4 -DARTIFACT_INFO=${ARTIFACT_INFO} -P ${CI_TOOLS}/make_artifact_name_env.cmake
)
