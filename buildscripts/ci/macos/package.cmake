
message(STATUS "Package")

# Config
set(ARTIFACTS_DIR "build.artifacts")
set(INSTALL_DIR "build.install")
set(PACKAGING_DIR ${CMAKE_CURRENT_LIST_DIR}/../../packaging/MacOS)

# Options
set(BUILD_MODE "" CACHE STRING "Build mode")
set(BUILD_VERSION "" CACHE STRING "Build mode")
set(SIGN_CERTIFICATE_ENCRYPT_SECRET "" CACHE STRING "Sign cert encrypt secret")
set(SIGN_CERTIFICATE_PASSWORD "" CACHE STRING "Sign cert pass")

if (NOT BUILD_MODE)
    file (STRINGS "${ARTIFACTS_DIR}/env/build_mode.env" BUILD_MODE)
endif()

if (NOT BUILD_VERSION)
    set(BUILD_VERSION 4.0.0.1234) # TODO
    #file (STRINGS "${ARTIFACTS_DIR}/env/build_version.env" BUILD_VERSION)
endif()

if (NOT SIGN_CERTIFICATE_ENCRYPT_SECRET)
    message(STATUS "not set SIGN_CERTIFICATE_ENCRYPT_SECRET")
endif()

if (NOT SIGN_CERTIFICATE_ENCRYPT_SECRET)
    message(STATUS "not set SIGN_CERTIFICATE_PASSWORD")
endif()

file(MAKE_DIRECTORY ${INSTALL_DIR}/audacity.app/Contents/Resources/Frameworks)

# Setup keychain for code sign
# if [ "$SIGN_CERTIFICATE_ENCRYPT_SECRET" != "''" ]; then

#     7z x -y ./buildscripts/ci/macos/resources/mac_musescore.p12.enc -o./buildscripts/ci/macos/resources/ -p${SIGN_CERTIFICATE_ENCRYPT_SECRET}

#     export CERTIFICATE_P12=./buildscripts/ci/macos/resources/mac_musescore.p12
#     export KEYCHAIN=build.keychain
#     security create-keychain -p ci $KEYCHAIN
#     security default-keychain -s $KEYCHAIN
#     security unlock-keychain -p ci $KEYCHAIN
#     # Set keychain timeout to 1 hour for long builds
#     # see http://www.egeek.me/2013/02/23/jenkins-and-xcode-user-interaction-is-not-allowed/
#     security set-keychain-settings -t 3600 -l $KEYCHAIN
#     security import $CERTIFICATE_P12 -k $KEYCHAIN -P "$SIGN_CERTIFICATE_PASSWORD" -T /usr/bin/codesign

#     security set-key-partition-list -S apple-tool:,apple: -s -k ci $KEYCHAIN
# fi

if (BUILD_MODE STREQUAL "nightly_build")
    #   BUILD_NUMBER=$(cat $ARTIFACTS_DIR/env/build_number.env)
    #   BUILD_BRANCH=$(cat $ARTIFACTS_DIR/env/build_branch.env)
    #   BUILD_REVISION=$(cat $ARTIFACTS_DIR/env/build_revision.env)
    #   ARTIFACT_NAME=AudacityNightly-${BUILD_NUMBER}-${BUILD_BRANCH}-${BUILD_REVISION}-${PACKARCH}
    set(LONG_NAME "AudacityNightly")
    set(ARTIFACT_NAME ${LONG_NAME}-${BUILD_VERSION}.dmg)
else()
    set(LONG_NAME "Audacity")
    set(ARTIFACT_NAME ${LONG_NAME}-${BUILD_VERSION}.dmg)
endif()

execute_process(
    COMMAND bash ${PACKAGING_DIR}/make_dmg.sh --long_name "${LONG_NAME}" --version ${BUILD_VERSION}
)

file(COPY_FILE ${INSTALL_DIR}/${ARTIFACT_NAME} ${ARTIFACTS_DIR}/${ARTIFACT_NAME})

# bash ./buildscripts/ci/tools/make_artifact_name_env.sh $ARTIFACT_NAME
