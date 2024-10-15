
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

# Install Qt
message(STATUS "=== Install Qt ===")

set(QT_ARCHIVE Qt624_msvc2019_64.7z)
set(QT_PATH ${BUILD_TOOLS}/6.2.4)
file(MAKE_DIRECTORY ${QT_PATH})
set(QT_URL https://s3.amazonaws.com/utils.musescore.org/${QT_ARCHIVE})

file(DOWNLOAD ${QT_URL} ${TEMP_DIR}/${QT_ARCHIVE} SHOW_PROGRESS)
message(STATUS "Extracting ${TEMP_DIR}/${QT_ARCHIVE}...")
file(ARCHIVE_EXTRACT INPUT ${TEMP_DIR}/${QT_ARCHIVE} DESTINATION ${QT_PATH})
message(STATUS "Extract finished")

file(APPEND ${ENV_FILE} "export QT_PATH=${QT_PATH}\n")
file(APPEND ${ENV_FILE} "export PATH=${QT_PATH}/msvc2019_64/bin:$PATH\n")

message(STATUS "Setup script done")
