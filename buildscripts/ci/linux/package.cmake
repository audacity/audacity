
message(STATUS "Package")

# Config
set(BUILD_TOOLS "$ENV{HOME}/build_tools")
set(ARTIFACTS_DIR "build.artifacts")
set(BUILD_DIR "build.release")

# Options
set(BUILD_MODE "" CACHE STRING "Build mode")
set(PACKARCH "x86_64" CACHE STRING "Package architecture")

if (NOT BUILD_MODE)
    file (STRINGS "${ARTIFACTS_DIR}/env/build_mode.env" BUILD_MODE)
endif()

if (NOT BUILD_MODE)
    message(FATAL_ERROR "not set BUILD_MODE")
endif()

include(${CMAKE_CURRENT_LIST_DIR}/../common/package_name.cmake)

file (STRINGS "${BUILD_DIR}/PREFIX.txt" INSTALL_DIR)

set(PACKTYPE "appimage")
# if [ "$BUILD_MODE" == "devel" ]; then PACKTYPE=appimage; fi
# if [ "$BUILD_MODE" == "nightly" ]; then PACKTYPE=appimage; fi
# if [ "$BUILD_MODE" == "testing" ]; then PACKTYPE=appimage; fi
# if [ "$BUILD_MODE" == "stable" ]; then PACKTYPE=appimage; fi


# echo "BUILD_MODE: $BUILD_MODE"
# echo "BUILD_VERSION: $BUILD_VERSION"
# echo "MAJOR_VERSION: $MAJOR_VERSION"
# echo "PACKTYPE: $PACKTYPE"
# echo "PACKARCH: $PACKARCH"
# echo "INSTALL_DIR: $INSTALL_DIR"

audacity_package_name(ARTIFACT_NAME linux ${PACKARCH} AppImage)

if(PACKTYPE STREQUAL "appimage")
    # To enable automatic updates for AppImages, set UPDATE_INFORMATION according to
    # https://github.com/AppImage/AppImageSpec/blob/master/draft.md#update-information
    # case "${BUILD_MODE}" in
    # "stable")  export UPDATE_INFORMATION="gh-releases-zsync|audacity|Audacity|latest|Audacity-*${PACKARCH}.AppImage.zsync";;
    # "nightly") export UPDATE_INFORMATION="zsync|https://ftp.osuosl.org/pub/audacity-nightlies/linux/${MAJOR_VERSION}x/nightly/AudacityNightly-latest-${BUILD_BRANCH}-${PACKARCH}.AppImage.zsync";;
    # *) unset UPDATE_INFORMATION;; # disable updates for other build modes
    # esac

    set(LOCAL_ROOT_PATH "${CMAKE_CURRENT_LIST_DIR}/_deps")
    get_filename_component(_repo_root "${CMAKE_CURRENT_LIST_DIR}/../../.." ABSOLUTE)
    include("${_repo_root}/muse_deps/buildtools/manifest.cmake")
    require_tool(appimagetool)
    require_tool(linuxdeploy)
    require_tool(linuxdeploy-plugin-qt)
    require_tool(appimageupdatetool)

    execute_process(
        COMMAND bash ${CMAKE_CURRENT_LIST_DIR}/make_appimage.sh "${INSTALL_DIR}" "${ARTIFACT_NAME}" "${PACKARCH}" "${BUILD_TOOLS}/environment.sh"
    )

    execute_process(
        COMMAND mv "${INSTALL_DIR}/../${ARTIFACT_NAME}" "${ARTIFACTS_DIR}/"
    )

    # Package file name, for the later CI steps (checksum, release)
    file(WRITE ${ARTIFACTS_DIR}/env/artifact_name.env ${ARTIFACT_NAME})


    # if [ -v UPDATE_INFORMATION ]; then
    #     # zsync file contains data for automatic delta updates
    #     mv "${INSTALL_DIR}/../${ARTIFACT_NAME}.AppImage.zsync" "${ARTIFACTS_DIR}/"
    # fi
endif()

message(STATUS "Package has finished!")
