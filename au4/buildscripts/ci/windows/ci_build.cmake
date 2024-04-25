
message(STATUS "Build")

# Config
set(ARTIFACTS_DIR "build.artifacts")
set(ROOT_DIR ${CMAKE_CURRENT_LIST_DIR}/../../..)

# Options
set(BUILD_NUMBER "" CACHE STRING "Build number")
set(BUILD_MODE "" CACHE STRING "Build mode")
set(BUILD_WIN_PORTABLE "" CACHE STRING "Build portable")

if (NOT BUILD_NUMBER)
    file (STRINGS "${ARTIFACTS_DIR}/env/build_number.env" BUILD_NUMBER)
endif()

if (NOT BUILD_MODE)
    file (STRINGS "${ARTIFACTS_DIR}/env/build_mode.env" BUILD_MODE)
endif()

set(APP_BUILD_MODE "dev")
if (BUILD_MODE STREQUAL "devel_build")
    set(APP_BUILD_MODE "dev")
elseif(BUILD_MODE STREQUAL "nightly_build")
    set(APP_BUILD_MODE "dev")
elseif(BUILD_MODE STREQUAL "testing_build")
    set(APP_BUILD_MODE "testing")
elseif(BUILD_MODE STREQUAL "stable_build")
    set(APP_BUILD_MODE "release")
endif()

# Setup VS 
# set(VSWHERE "C:\Program Files (x86)\Microsoft Visual Studio\Installer\vswhere.exe")
# execute_process(
#     COMMAND ${VSWHERE} -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath
#     OUTPUT_VARIABLE VS_INSTALL_DIR
# )
# message("VS_INSTALL_DIR: ${VS_INSTALL_DIR}")
# execute_process(
#     COMMAND cmd "${VS_INSTALL_DIR}\VC\Auxiliary\Build\vcvars64.bat"
# )


# ECHO "Setup VS Environment"
# SET VSWHERE="C:\Program Files (x86)\Microsoft Visual Studio\Installer\vswhere.exe"
# FOR /f "usebackq tokens=*" %%i in (`%VSWHERE% -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath`) do (
#   SET VS_INSTALL_DIR=%%i
# )
# ECHO "VS_INSTALL_DIR: %VS_INSTALL_DIR%"
# CALL "%VS_INSTALL_DIR%\VC\Auxiliary\Build\vcvars64.bat"



# Build
set(CONFIG
    -DBUILD_TYPE=release_install
    -DBUILD_MODE=${APP_BUILD_MODE}
    -DBUILD_NUMBER=${BUILD_NUMBER}
)

execute_process(
    COMMAND "${ROOT_DIR}/ci_build.bat" ${CONFIG}
    RESULT_VARIABLE BUILD_RESULT
)

if (BUILD_RESULT GREATER 0)
    message(FATAL_ERROR "Failed build")
endif()


# bash ./buildscripts/ci/tools/make_release_channel_env.sh -c %MUSE_APP_BUILD_MODE%
# bash ./buildscripts/ci/tools/make_version_env.sh %BUILD_NUMBER%
# bash ./buildscripts/ci/tools/make_branch_env.sh
