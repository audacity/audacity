
# Config
set(ARTIFACTS_DIR "build.artifacts")
set(CI_DIR ${CMAKE_CURRENT_LIST_DIR}/..)
set(CI_TOOLS ${CI_DIR}/tools)

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

# Make artifact name env
execute_process(
    COMMAND cmake -DPREFIX=AU4 -DARTIFACT_INFO=${ARTIFACT_INFO} -P ${CI_TOOLS}/make_artifact_name_env.cmake
)




