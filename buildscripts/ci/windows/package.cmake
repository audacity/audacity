
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

# arch controls
set(TARGET_PROCESSOR_BITS "64" CACHE STRING "32 or 64")
if(TARGET_PROCESSOR_BITS STREQUAL "32")
  set(TARGET_PROCESSOR_ARCH "x86")
  set(_WIX_PACK_DIR "win32")
else()
  set(TARGET_PROCESSOR_ARCH "x86_64")
  set(_WIX_PACK_DIR "win64")
endif()

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
elseif(BUILD_MODE STREQUAL "stable_build")
    set(PACK_TYPE "msi")
endif()

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
  message(STATUS "UPGRADE_UUID: ${UPGRADE_UUID}")

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
    if(TARGET_PROCESSOR_BITS STREQUAL "32")
      set(_features "${BUILD_DIR}/_CPack_Packages/win32/WIX/features.wxs")
    endif()

    if(EXISTS "${_features}")
      message(STATUS "---- features.wxs (start) ----")
      execute_process(COMMAND "${CMAKE_COMMAND}" -E cat "${_features}")
      message(STATUS "---- features.wxs (end) ----")
    else()
      message(WARNING "features.wxs not found at ${_features}")
    endif()

    message(FATAL_ERROR "CPack/WiX packaging failed")
  endif()

  if(TARGET_PROCESSOR_BITS STREQUAL "32")
    set(_WIX_PACK_DIR "win32")
  else()
    set(_WIX_PACK_DIR "win64")
  endif()
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
      set(ARTIFACT_NAME "Audacity-Nightly-${BUILD_NUMBER}-${BUILD_BRANCH}-${BUILD_REVISION}-${TARGET_PROCESSOR_ARCH}.msi")
    else()
      set(ARTIFACT_NAME "Audacity-${BUILD_VERSION}-${TARGET_PROCESSOR_ARCH}.msi")
    endif()
  else()
    set(ARTIFACT_NAME "Audacity-${BUILD_VERSION}-${TARGET_PROCESSOR_ARCH}.msi")
  endif()

  set(ARTIFACT_PATH "${ARTIFACTS_DIR}/${ARTIFACT_NAME}")
  file(COPY "${MSI_FILE}" DESTINATION "${ARTIFACTS_DIR}")
  get_filename_component(_copied_name "${MSI_FILE}" NAME)
  file(RENAME "${ARTIFACTS_DIR}/${_copied_name}" "${ARTIFACT_PATH}")
  message(STATUS "Copied installer to ${ARTIFACT_PATH}")

  message(STATUS "Finished MSI packing")
  return()
endif()
