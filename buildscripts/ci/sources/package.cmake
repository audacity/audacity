
# Package the sources: the tracked files of the repository and its submodules
# with the dependency sources of an offline build, in the layout of the
# audacity-sources-4.0.2.tar.xz release asset:
#
#   audacity-<version>/              every tracked file, no .git
#   audacity-<version>/offline-deps/ PrepareDepsSources.cmake's cache, crashpad left out
#   audacity-<version>/OFFLINE_BUILD.md
#
# Run from the repository root after ci_configure.cmake:
#   cmake -P buildscripts/ci/sources/package.cmake

set(ARTIFACTS_DIR "build.artifacts")
set(STAGING_DIR "build.sources")

get_filename_component(ROOT_DIR "${CMAKE_CURRENT_LIST_DIR}/../../.." ABSOLUTE)
include(${CMAKE_CURRENT_LIST_DIR}/../common/audacity_package_name.cmake)
include(${ROOT_DIR}/version.cmake)

audacity_package_name(ARCHIVE_NAME sources "" tar.xz)
string(REGEX REPLACE "\\.tar\\.xz$" "" TOP_DIR "${ARCHIVE_NAME}")
string(REPLACE "-sources-" "-" TOP_DIR "${TOP_DIR}") # audacity-4.0.2, audacity-nightly-4.0.2.260831071
message(STATUS "Packaging the sources as ${ARCHIVE_NAME}, top directory ${TOP_DIR}")

set(SOURCES_DIR "${STAGING_DIR}/${TOP_DIR}")
file(REMOVE_RECURSE "${STAGING_DIR}")
file(MAKE_DIRECTORY "${SOURCES_DIR}")

# The tracked files, submodules included
execute_process(
    COMMAND git ls-files --recurse-submodules
    OUTPUT_FILE "${STAGING_DIR}/files.txt"
    RESULT_VARIABLE rc
)
if (NOT rc EQUAL 0)
    message(FATAL_ERROR "git ls-files failed (exit ${rc})")
endif()
execute_process(
    COMMAND rsync --archive --files-from=${STAGING_DIR}/files.txt . ${SOURCES_DIR}
    RESULT_VARIABLE rc
)
if (NOT rc EQUAL 0)
    message(FATAL_ERROR "rsync failed (exit ${rc})")
endif()

# The dependency sources, for a build without network
execute_process(
    COMMAND ${CMAKE_COMMAND}
        -DEXTDEPS_CACHE=${ROOT_DIR}/${SOURCES_DIR}/offline-deps
        -DMUSE_MODULE_DIAGNOSTICS_CRASHPAD_CLIENT=OFF
        -P ${ROOT_DIR}/buildscripts/cmake/PrepareDepsSources.cmake
    RESULT_VARIABLE rc
)
if (NOT rc EQUAL 0)
    message(FATAL_ERROR "PrepareDepsSources.cmake failed (exit ${rc})")
endif()

# The note of the snapshot, OFFLINE_BUILD.md as in the 4.0.2 archive
execute_process(
    COMMAND git rev-parse HEAD
    OUTPUT_VARIABLE COMMIT
    OUTPUT_STRIP_TRAILING_WHITESPACE
    RESULT_VARIABLE rc
)
string(LENGTH "${COMMIT}" COMMIT_LENGTH)
if (NOT rc EQUAL 0 OR NOT COMMIT MATCHES "^[0-9a-f]+$" OR NOT COMMIT_LENGTH EQUAL 40) # CMake regexes have no {40}
    message(FATAL_ERROR "git rev-parse HEAD failed (exit ${rc}): '${COMMIT}'")
endif()
string(SUBSTRING "${COMMIT}" 0 12 COMMIT_SHORT)
set(VERSION "${MUSE_APP_VERSION}")
if (MUSE_APP_VERSION_LABEL)
    string(APPEND VERSION "-${MUSE_APP_VERSION_LABEL}")
endif()
file(WRITE "${SOURCES_DIR}/OFFLINE_BUILD.md" "# Audacity ${VERSION} source snapshot

This source tree was created from audacity/audacity commit `${COMMIT}`.

Dependency source archives and source-delivery repositories are in `offline-deps/downloads`.

To build using the bundled dependency source cache:

```sh
export EXTDEPS_CACHE=\"$PWD/offline-deps\"
cmake -S . -B build -DEXTDEPS_CACHE=\"$EXTDEPS_CACHE\" -DMUSE_MODULE_DIAGNOSTICS_CRASHPAD_CLIENT=OFF # add the normal build options for your platform
```

Snapshot revision: ${COMMIT_SHORT}
")

# The archive
message(STATUS "Compressing ${ARCHIVE_NAME}...")
set(ENV{XZ_OPT} "-T0")
execute_process(
    COMMAND tar --create --xz --file ${ROOT_DIR}/${ARTIFACTS_DIR}/${ARCHIVE_NAME} ${TOP_DIR}
    WORKING_DIRECTORY ${STAGING_DIR}
    RESULT_VARIABLE rc
)
if (NOT rc EQUAL 0)
    message(FATAL_ERROR "tar failed (exit ${rc})")
endif()
file(REMOVE_RECURSE "${STAGING_DIR}")

# Package file name, for the later CI steps (checksum, release)
file(WRITE ${ARTIFACTS_DIR}/env/artifact_name.env ${ARCHIVE_NAME})
message(STATUS "Packaged ${ARTIFACTS_DIR}/${ARCHIVE_NAME}")
