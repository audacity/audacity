
# Public file names of the packages, as published on GitHub releases and in the
# updater feed (4.0.0 and 4.0.1 set the scheme):
#
#   audacity-<os>-<version>-<arch>.<ext>        audacity-linux-4.0.1-x86_64.AppImage
#                                               audacity-macOS-4.0.1-universal.dmg
#                                               audacity-win-4.0.1-arm64.msi
#
# <version> is MUSE_APP_VERSION from version.cmake, followed by -<label> for a
# pre-release (MUSE_APP_VERSION_LABEL). Nightly builds get a "nightly" marker and
# the build number, so that every nightly has a name of its own:
#
#   audacity-nightly-<os>-<version>.<build number>-<arch>.<ext>
#
# <arch> is the PACKARCH of the build, except that Windows on ARM is published
# as arm64 (Linux keeps aarch64).
#
# Usage, from a package script run with cmake -P:
#   include(${CMAKE_CURRENT_LIST_DIR}/../common/package_name.cmake)
#   audacity_package_name(ARTIFACT_NAME linux ${PACKARCH} AppImage)

function(audacity_package_name OUT_VAR OS ARCH EXT)
    if (NOT ARTIFACTS_DIR)
        set(ARTIFACTS_DIR "build.artifacts")
    endif()
    if (NOT BUILD_MODE)
        file(STRINGS "${ARTIFACTS_DIR}/env/build_mode.env" BUILD_MODE)
    endif()
    if (NOT BUILD_MODE)
        message(FATAL_ERROR "not set BUILD_MODE")
    endif()

    include(${CMAKE_CURRENT_FUNCTION_LIST_DIR}/../../../version.cmake)

    set(VERSION "${MUSE_APP_VERSION}")
    if (MUSE_APP_VERSION_LABEL)
        set(VERSION "${VERSION}-${MUSE_APP_VERSION_LABEL}")
    endif()

    set(NAME "audacity")
    if (BUILD_MODE STREQUAL "nightly")
        file(STRINGS "${ARTIFACTS_DIR}/env/build_number.env" BUILD_NUMBER)
        if (NOT BUILD_NUMBER)
            message(FATAL_ERROR "not set BUILD_NUMBER")
        endif()
        set(NAME "audacity-nightly")
        set(VERSION "${VERSION}.${BUILD_NUMBER}")
    endif()

    if (OS STREQUAL "win" AND ARCH STREQUAL "aarch64")
        set(ARCH "arm64")
    endif()

    set(${OUT_VAR} "${NAME}-${OS}-${VERSION}-${ARCH}.${EXT}" PARENT_SCOPE)
endfunction()