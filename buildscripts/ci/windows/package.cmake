
message(STATUS "Package")

# Config
set(ARTIFACTS_DIR "build.artifacts")
set(BUILD_DIR "build.release")
set(INSTALL_DIR "build.install")

# Options
set(BUILD_MODE "" CACHE STRING "Build mode")
set(BUILD_VERSION "" CACHE STRING "Build mode")
set(BUILD_NUMBER   "" CACHE STRING "Nightly build number")
set(BUILD_BRANCH   "" CACHE STRING "Nightly branch")
set(BUILD_REVISION "" CACHE STRING "Nightly revision (short SHA)")

set(UPGRADE_UUID "11111111-1111-1111-1111-111111111111" CACHE STRING "WiX Upgrade GUID")

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
  if(DEFINED ENV{WIN_MSI_TESTING_AU4_GUID} AND NOT "$ENV{WIN_MSI_TESTING_AU4_GUID}" STREQUAL "")
    set(UPGRADE_UUID "$ENV{WIN_MSI_TESTING_AU4_GUID}" CACHE STRING "WiX Upgrade GUID" FORCE)
  endif()
elseif(BUILD_MODE STREQUAL "stable_build")
  set(PACK_TYPE "msi")
  if(DEFINED ENV{WIN_MSI_STABLE_AU4_GUID} AND NOT "$ENV{WIN_MSI_STABLE_AU4_GUID}" STREQUAL "")
    set(UPGRADE_UUID "$ENV{WIN_MSI_STABLE_AU4_GUID}" CACHE STRING "WiX Upgrade GUID" FORCE)
  endif()
endif()

# Setup signing
set(SIGN_KEY    "$ENV{SIGN_KEY}")
set(SIGN_SECRET "$ENV{SIGN_SECRET}")

if(NOT DEFINED SIGN_ENABLE AND SIGN_KEY AND SIGN_SECRET)
  set(SIGN_ENABLE ON)
endif()

set(SIGN_SERVICE_SH "${CMAKE_SOURCE_DIR}/buildscripts/ci/windows/sign_service_aws.sh")

file(MAKE_DIRECTORY "${ARTIFACTS_DIR}")

# PACK 7z
if(PACK_TYPE STREQUAL "7z")
  message(STATUS "Start 7z packing...")
  set(ARTIFACT_NAME "Audacity-${BUILD_VERSION}-x86_64")

  file(RENAME ${INSTALL_DIR} ${ARTIFACT_NAME})
  file(ARCHIVE_CREATE OUTPUT ${ARTIFACTS_DIR}/${ARTIFACT_NAME}.7z PATHS ${ARTIFACT_NAME} FORMAT 7zip)

message(STATUS "Finished 7z packing")
endif()

# PACK MSI
if(PACK_TYPE STREQUAL "msi")
  message(STATUS "Start MSI packing...")

  set(PACKAGE_UUID "")
  if(WIN32)
    execute_process(
      COMMAND powershell -NoProfile -Command "[guid]::NewGuid().ToString()"
      OUTPUT_VARIABLE PACKAGE_UUID
      OUTPUT_STRIP_TRAILING_WHITESPACE
      ERROR_QUIET
    )
  endif()
  if(NOT PACKAGE_UUID)
    message(FATAL_ERROR "Could not generate PACKAGE_UUID")
  endif()
  message(STATUS "PACKAGE_UUID: ${PACKAGE_UUID}")

  execute_process(
    COMMAND "${CMAKE_COMMAND}"
      -S "${CMAKE_SOURCE_DIR}" -B "${BUILD_DIR}"
      -DCPACK_WIX_PRODUCT_GUID=${PACKAGE_UUID}
      -DCPACK_WIX_UPGRADE_GUID=${UPGRADE_UUID}
    RESULT_VARIABLE _rc_conf
  )
  if(NOT _rc_conf EQUAL 0)
    message(FATAL_ERROR "CMake reconfigure failed for WiX/CPack injection")
  endif()

  set(_cfg_arg "")
  if(BUILD_MODE MATCHES "^[Rr]elease|[Dd]ebug|RelWithDebInfo|MinSizeRel$")
    set(_cfg_arg "--config" "${BUILD_MODE}")
  else()
    set(_cfg_arg "--config" "Release")
  endif()

  set(_app_target "audacity")
  execute_process(
    COMMAND "${CMAKE_COMMAND}" --build "${BUILD_DIR}" --target ${_app_target} ${_cfg_arg}
    RESULT_VARIABLE _rc_build_app
  )
  if(NOT _rc_build_app EQUAL 0)
    message(FATAL_ERROR "Failed to build target ${_app_target} for signing")
  endif()

  if(SIGN_ENABLE AND SIGN_KEY AND SIGN_SECRET)
    set(_app_exe "${BUILD_DIR}/Audacity4.exe")

    if(EXISTS "${_app_exe}")
      message(STATUS "[sign-prepack] exe: ${_app_exe}")
      find_program(BASH_EXECUTABLE bash REQUIRED)
      execute_process(
        COMMAND "${BASH_EXECUTABLE}" "${SIGN_SERVICE_SH}"
                --s3_key     "${SIGN_KEY}"
                --s3_secret  "${SIGN_SECRET}"
                --file_path  "${_app_exe}"
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
        RESULT_VARIABLE _rc_exe
      )
      if(NOT _rc_exe EQUAL 0)
        message(WARNING "[sign-prepack] failed to sign ${_app_exe} (rc=${_rc_exe})")
      endif()
    else()
      message(WARNING "[sign-prepack] exe not found at ${_app_exe}")
    endif()
  else()
    message(STATUS "[sign-prepack] disabled or credentials missing; skipping exe signing")
  endif()

  execute_process(
    COMMAND "${CMAKE_COMMAND}" --build "${BUILD_DIR}" --target package ${_cfg_arg}
    RESULT_VARIABLE _rc_pkg
  )
  if(NOT _rc_pkg EQUAL 0)
    set(_wix_log64 "${BUILD_DIR}/_CPack_Packages/win64/WIX/wix.log")
    set(_wix_log32 "${BUILD_DIR}/_CPack_Packages/win32/WIX/wix.log")
    foreach(_log "${_wix_log64}" "${_wix_log32}")
      if(EXISTS "${_log}")
        message(STATUS "---- wix.log (start) ----")
        execute_process(COMMAND "${CMAKE_COMMAND}" -E cat "${_log}")
        message(STATUS "---- wix.log (end) ----")
      endif()
    endforeach()

    set(_features "${BUILD_DIR}/_CPack_Packages/win64/WIX/features.wxs")

    if(EXISTS "${_features}")
      message(STATUS "---- features.wxs (start) ----")
      execute_process(COMMAND "${CMAKE_COMMAND}" -E cat "${_features}")
      message(STATUS "---- features.wxs (end) ----")
    else()
      message(WARNING "features.wxs not found at ${_features}")
    endif()

    message(FATAL_ERROR "CPack/WiX packaging failed")
  endif()

  set(_WIX_PACK_DIR "win64")
  set(_wix_logs_src "${BUILD_DIR}/_CPack_Packages/${_WIX_PACK_DIR}/WIX")
  if(EXISTS "${_wix_logs_src}")
    set(_wix_logs_dst "${ARTIFACTS_DIR}/logs/WIX")
    file(MAKE_DIRECTORY "${_wix_logs_dst}")
    file(COPY "${_wix_logs_src}/" DESTINATION "${_wix_logs_dst}")
    file(GLOB _msis_in_logs "${_wix_logs_dst}/*.msi")
    foreach(_m ${_msis_in_logs})
      file(REMOVE "${_m}")
    endforeach()
    message(STATUS "Copied WiX logs to ${_wix_logs_dst}")
  endif()

  file(GLOB_RECURSE _msi_candidates "${BUILD_DIR}/*.msi")
  list(LENGTH _msi_candidates _n)
  if(NOT _n EQUAL 1)
    message(WARNING "Expected exactly 1 MSI, found ${_n}")
  endif()
  list(GET _msi_candidates 0 MSI_FILE)
  if(NOT MSI_FILE)
    message(FATAL_ERROR "No MSI produced in ${BUILD_DIR}")
  endif()

  if(BUILD_MODE STREQUAL "nightly_build")
    if(BUILD_NUMBER AND BUILD_BRANCH AND BUILD_REVISION)
      set(ARTIFACT_NAME "Audacity-Nightly-${BUILD_NUMBER}-${BUILD_BRANCH}-${BUILD_REVISION}-x86_64.msi")
    else()
      set(ARTIFACT_NAME "Audacity-${BUILD_VERSION}-x86_64.msi")
    endif()
  else()
    set(ARTIFACT_NAME "Audacity-${BUILD_VERSION}-x86_64.msi")
  endif()

  set(ARTIFACT_PATH "${ARTIFACTS_DIR}/${ARTIFACT_NAME}")
  file(COPY "${MSI_FILE}" DESTINATION "${ARTIFACTS_DIR}")
  get_filename_component(_copied_name "${MSI_FILE}" NAME)
  file(RENAME "${ARTIFACTS_DIR}/${_copied_name}" "${ARTIFACT_PATH}")
  message(STATUS "Copied installer to ${ARTIFACT_PATH}")

  if(SIGN_ENABLE)
    if(NOT EXISTS "${SIGN_SERVICE_SH}")
      message(FATAL_ERROR "Signing script not found: ${SIGN_SERVICE_SH}")
    endif()

    find_program(BASH_EXECUTABLE bash)

    message(STATUS "Signing MSI: ${ARTIFACT_PATH}")
    execute_process(
      COMMAND "${BASH_EXECUTABLE}" "${SIGN_SERVICE_SH}"
              --s3_key "${SIGN_KEY}"
              --s3_secret "${SIGN_SECRET}"
              --file_path "${ARTIFACT_PATH}"
      WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
      RESULT_VARIABLE _sign_rc
    )
    if(NOT _sign_rc EQUAL 0)
      message(FATAL_ERROR "Code signing failed (exit ${_sign_rc})")
    endif()
    message(STATUS "Signing complete: ${ARTIFACT_PATH}")
  endif()

  message(STATUS "Finished MSI packing")
  return()
endif()
