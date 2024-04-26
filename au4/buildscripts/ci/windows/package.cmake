
message(STATUS "Package")

# Config
set(ARTIFACTS_DIR "build.artifacts")
set(BUILD_DIR "build.release")
set(INSTALL_DIR "build.install")

# Options
set(BUILD_MODE "" CACHE STRING "Build mode")
set(BUILD_VERSION "" CACHE STRING "Build mode")

if (NOT BUILD_MODE)
    file (STRINGS "${ARTIFACTS_DIR}/env/build_mode.env" BUILD_MODE)
endif()

# Check options
if (NOT BUILD_MODE)
    message(FATAL_ERROR "not set BUILD_MODE")
endif()

if (NOT BUILD_VERSION)
    file (STRINGS "${ARTIFACTS_DIR}/env/build_version.env" BUILD_VERSION)
endif()

# Setup package type
set(PACK_TYPE "7z")
if (BUILD_MODE STREQUAL "devel_build")
    set(PACK_TYPE "7z")
elseif(BUILD_MODE STREQUAL "nightly_build")
    set(PACK_TYPE "7z")
elseif(BUILD_MODE STREQUAL "testing_build")
    set(PACK_TYPE "msi")
elseif(BUILD_MODE STREQUAL "stable_build")
    set(PACK_TYPE "msi")
endif()


# PACK 7z
message(STATUS "Start 7z packing...")
set(ARTIFACT_NAME "Audacity-${BUILD_VERSION}-x86_64")

file(RENAME ${INSTALL_DIR} ${ARTIFACT_NAME})
file(ARCHIVE_CREATE OUTPUT ${ARTIFACTS_DIR}/${ARTIFACT_NAME}.7z PATHS ${ARTIFACT_NAME} FORMAT 7zip)

message(STATUS "Finished 7z packing")


