
# Public file names of Audacity's packages, as published on GitHub releases and
# in the updater feed (4.0.0 and 4.0.1 set the scheme):
#
#   audacity-<os>-<version>-<arch>.<ext>        audacity-linux-4.0.1-x86_64.AppImage
#                                               audacity-macOS-4.0.1-universal.dmg
#                                               audacity-win-4.0.1-arm64.msi
#   audacity-sources-<version>.tar.xz           the source archive, no arch
#
# <version> is MUSE_APP_VERSION from version.cmake, followed by -<label> for a
# pre-release (MUSE_APP_VERSION_LABEL). Nightly builds get a "nightly" marker and
# the build number, so that every nightly has a name of its own:
#
#   audacity-nightly-<os>-<version>.<build number>-<arch>.<ext>
#   audacity-nightly-sources-<version>.<build number>.tar.xz
#
# <arch> is the PACKARCH of the build, except that Windows on ARM is published
# as arm64 (Linux keeps aarch64).
#
# This file holds what is Audacity's; the composing is muse_package_name() in
# package_name.cmake, which has nothing app-specific in it.
#
# Usage, from a package script run with cmake -P:
#   include(${CMAKE_CURRENT_LIST_DIR}/../common/audacity_package_name.cmake)
#   audacity_package_name(ARTIFACT_NAME linux ${PACKARCH} AppImage)
#   audacity_package_name(ARCHIVE_NAME sources "" tar.xz)

include(${CMAKE_CURRENT_LIST_DIR}/package_name.cmake)

function(audacity_package_name OUT_VAR OS ARCH EXT)
    include(${CMAKE_CURRENT_FUNCTION_LIST_DIR}/../../../version.cmake)

    set(VERSION "${MUSE_APP_VERSION}")
    if (MUSE_APP_VERSION_LABEL)
        string(APPEND VERSION "-${MUSE_APP_VERSION_LABEL}")
    endif()

    if (OS STREQUAL "win" AND ARCH STREQUAL "aarch64")
        set(ARCH "arm64")
    endif()

    if (OS STREQUAL "sources") # the source archive has no arch
        set(PATTERN "{app}-{os}-{version}.{ext}")
        set(NIGHTLY_PATTERN "{app}-nightly-{os}-{version}.{build}.{ext}")
    else()
        set(PATTERN "{app}-{os}-{version}-{arch}.{ext}")
        set(NIGHTLY_PATTERN "{app}-nightly-{os}-{version}.{build}-{arch}.{ext}")
    endif()

    # BUILD_MODE and ARTIFACTS_DIR are the calling package script's; when it has
    # none, muse_package_name() falls back to build.artifacts/env/build_mode.env.
    set(_from_caller)
    if (BUILD_MODE)
        list(APPEND _from_caller MODE "${BUILD_MODE}")
    endif()
    if (ARTIFACTS_DIR)
        list(APPEND _from_caller ARTIFACTS_DIR "${ARTIFACTS_DIR}")
    endif()
    if (ARCH)
        list(APPEND _from_caller ARCH "${ARCH}")
    endif()

    muse_package_name(NAME
        APP audacity
        VERSION "${VERSION}"
        PATTERN "${PATTERN}"
        NIGHTLY_PATTERN "${NIGHTLY_PATTERN}"
        OS "${OS}" EXT "${EXT}"
        ${_from_caller}
    )
    set(${OUT_VAR} "${NAME}" PARENT_SCOPE)
endfunction()
