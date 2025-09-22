
message(STATUS "Package")

# Config
set(ARTIFACTS_DIR "build.artifacts")
set(INSTALL_DIR "build.install")
set(PACKAGING_DIR ${CMAKE_CURRENT_LIST_DIR}/../../packaging/MacOS)
set(ROOT_DIR ${CMAKE_CURRENT_LIST_DIR}/../../..)

# Options
set(BUILD_MODE "" CACHE STRING "Build mode")
set(BUILD_VERSION "" CACHE STRING "Build mode")

if (NOT BUILD_MODE)
    file (STRINGS "${ARTIFACTS_DIR}/env/build_mode.env" BUILD_MODE)
endif()

include(${ROOT_DIR}/version.cmake)

if (NOT BUILD_VERSION)
    set(BUILD_VERSION ${MUSE_APP_VERSION})
    #file (STRINGS "${ARTIFACTS_DIR}/env/build_version.env" BUILD_VERSION)
endif()

file(MAKE_DIRECTORY ${INSTALL_DIR}/audacity.app/Contents/Resources/Frameworks)

# Setup keychain for code sign
set(_secret "$ENV{SIGN_CERTIFICATE_ENCRYPT_SECRET}")
if(_secret)
  set(_out_dir  "${CMAKE_SOURCE_DIR}/muse_framework/buildscripts/ci/macos/resources")
  set(_p12_enc  "${_out_dir}/mac_musescore.p12.enc")
  set(_p12      "${_out_dir}/mac_musescore.p12")
  set(_keychain "build.keychain")

  find_program(SEVENZ   7z        REQUIRED)
  find_program(SECURITY security  REQUIRED)

  execute_process(
    COMMAND "${SEVENZ}" x -y "${_p12_enc}" "-o${_out_dir}" "-p${_secret}"
    RESULT_VARIABLE _rc
  )
  if(NOT _rc EQUAL 0 OR NOT EXISTS "${_p12}")
    message(FATAL_ERROR "Failed to decrypt/import ${_p12_enc}")
  endif()

  function(_sec)
    execute_process(COMMAND "${SECURITY}" ${ARGV} RESULT_VARIABLE _sx)
    if(NOT _sx EQUAL 0)
      string(REPLACE ";" " " _cmd "${ARGV}")
      message(FATAL_ERROR "security ${_cmd} failed (${_sx})")
    endif()
  endfunction()

  _sec(create-keychain -p ci "${_keychain}")
  _sec(default-keychain -s "${_keychain}")
  _sec(unlock-keychain -p ci "${_keychain}")
  _sec(set-keychain-settings -t 3600 -l "${_keychain}")

  _sec(import "${_p12}" -k "${_keychain}" -P "$ENV{SIGN_CERTIFICATE_PASSWORD}" -T /usr/bin/codesign)
  _sec(set-key-partition-list -S apple-tool:,apple: -s -k ci "${_keychain}")

  message(STATUS "[mac signing] keychain ready")
else()
  message(STATUS "[mac signing] SIGN_CERTIFICATE_ENCRYPT_SECRET not set; skipping keychain setup")
endif()

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

file(MAKE_DIRECTORY ${ARTIFACTS_DIR}/env)

execute_process(
    COMMAND bash ${CMAKE_SOURCE_DIR}/buildscripts/ci/tools/make_artifact_name_env.sh ${ARTIFACT_NAME}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
