
message(STATUS "Package")

# Config
set(BUILD_TOOLS "$ENV{HOME}/build_tools")
set(ARTIFACTS_DIR "build.artifacts")
set(BUILD_DIR "build.release")

# Options
set(BUILD_MODE "" CACHE STRING "Build mode")
set(BUILD_VERSION "" CACHE STRING "Build mode")
set(PACKARCH "x86_64" CACHE STRING "Build mode")

if (NOT BUILD_MODE)
    file (STRINGS "${ARTIFACTS_DIR}/env/build_mode.env" BUILD_MODE)
endif()

if (NOT BUILD_VERSION)
    file (STRINGS "${ARTIFACTS_DIR}/env/build_version.env" BUILD_VERSION)
endif()

if (NOT BUILD_MODE)
    message(FATAL_ERROR "not set BUILD_MODE")
endif()

if (NOT BUILD_VERSION)
    message(FATAL_ERROR "not set BUILD_VERSION")
endif()

file (STRINGS "${BUILD_DIR}/PREFIX.txt" INSTALL_DIR)

set(PACKTYPE "appimage")
# if [ "$BUILD_MODE" == "devel_build" ]; then PACKTYPE=appimage; fi
# if [ "$BUILD_MODE" == "nightly_build" ]; then PACKTYPE=appimage; fi
# if [ "$BUILD_MODE" == "testing_build" ]; then PACKTYPE=appimage; fi
# if [ "$BUILD_MODE" == "stable_build" ]; then PACKTYPE=appimage; fi


# echo "BUILD_MODE: $BUILD_MODE"
# echo "BUILD_VERSION: $BUILD_VERSION"
# echo "MAJOR_VERSION: $MAJOR_VERSION"
# echo "PACKTYPE: $PACKTYPE"
# echo "PACKARCH: $PACKARCH"
# echo "INSTALL_DIR: $INSTALL_DIR"

if (BUILD_MODE STREQUAL "nightly_build")
    #   BUILD_NUMBER=$(cat $ARTIFACTS_DIR/env/build_number.env)
    #   BUILD_BRANCH=$(cat $ARTIFACTS_DIR/env/build_branch.env)
    #   BUILD_REVISION=$(cat $ARTIFACTS_DIR/env/build_revision.env)
    #   ARTIFACT_NAME=AudacityNightly-${BUILD_NUMBER}-${BUILD_BRANCH}-${BUILD_REVISION}-${PACKARCH}
    set(ARTIFACT_NAME "AudacityNightly-${BUILD_VERSION}-${PACKARCH}")
else()
    set(ARTIFACT_NAME "Audacity-${BUILD_VERSION}-${PACKARCH}")
endif()

# if [ "$PACKTYPE" == "7z" ]; then
#     mv $INSTALL_DIR $ARTIFACT_NAME
#     7z a $ARTIFACTS_DIR/$ARTIFACT_NAME.7z $ARTIFACT_NAME
#     bash ./buildscripts/ci/tools/make_artifact_name_env.sh $ARTIFACT_NAME.7z
#     chmod a+rw $ARTIFACT_NAME.7z
# fi

if(PACKTYPE STREQUAL "appimage")
    # To enable automatic updates for AppImages, set UPDATE_INFORMATION according to
    # https://github.com/AppImage/AppImageSpec/blob/master/draft.md#update-information
    # case "${BUILD_MODE}" in
    # "stable_build")  export UPDATE_INFORMATION="gh-releases-zsync|musescore|MuseScore|latest|MuseScore-*${PACKARCH}.AppImage.zsync";;
    # "nightly_build") export UPDATE_INFORMATION="zsync|https://ftp.osuosl.org/pub/musescore-nightlies/linux/${MAJOR_VERSION}x/nightly/MuseScoreNightly-latest-${BUILD_BRANCH}-${PACKARCH}.AppImage.zsync";;
    # *) unset UPDATE_INFORMATION;; # disable updates for other build modes
    # esac

    execute_process(
        COMMAND bash ${CMAKE_CURRENT_LIST_DIR}/make_appimage.sh "${INSTALL_DIR}" "${ARTIFACT_NAME}.AppImage" "${PACKARCH}" "${BUILD_TOOLS}/environment.sh"
    )

    execute_process(
        COMMAND mv "${INSTALL_DIR}/../${ARTIFACT_NAME}.AppImage" "${ARTIFACTS_DIR}/"
    )


    # if [ -v UPDATE_INFORMATION ]; then
    #     # zsync file contains data for automatic delta updates
    #     mv "${INSTALL_DIR}/../${ARTIFACT_NAME}.AppImage.zsync" "${ARTIFACTS_DIR}/"
    # fi
endif()

message(STATUS "Package has finished!")

