
message(STATUS "Setup Windows build environment")

# Config
set(BUILD_TOOLS "c:/build_tools")
set(ENV_FILE ${BUILD_TOOLS}/environment.sh)

# Options
set(BUILD_WIN_PORTABLE "" CACHE STRING "Build number")
option(BUILD_WIN_PORTABLE "Build app portable" OFF)


# Install tools
execute_process(COMMAND choco install -y git.install)
execute_process(COMMAND choco install -y wget)
execute_process(COMMAND choco install -y 7zip.install)

# Set dir
file(REMOVE ${ENV_FILE})

set(TEMP_DIR "${BUILD_TOOLS}/temp")
file(MAKE_DIRECTORY ${TEMP_DIR})

file(WRITE ${ENV_FILE} "")

message(STATUS "Setup script done")
