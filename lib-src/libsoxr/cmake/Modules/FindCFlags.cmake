# SoX Resampler Library       Copyright (c) 2007-16 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

# - Function to find C compiler feature flags

include (CheckCSourceCompiles)
include (FindPackageHandleStandardArgs)

function (FindCFlags PKG_NAME PKG_DESC TRIAL_C_FLAGS TEST_C_SOURCE)

foreach (TRIAL_C_FLAG ${TRIAL_C_FLAGS})
  message (STATUS "Trying ${PKG_NAME} C flags: ${TRIAL_C_FLAG}")
  unset (DETECT_${PKG_NAME}_C_FLAGS CACHE) #displayed by check_c_source_compiles

  set (TMP "${CMAKE_REQUIRED_FLAGS}")
  set (CMAKE_REQUIRED_FLAGS "${TRIAL_C_FLAG}")
  check_c_source_compiles ("${TEST_C_SOURCE}" DETECT_${PKG_NAME}_C_FLAGS)
  set (CMAKE_REQUIRED_FLAGS "${TMP}")

  if (DETECT_${PKG_NAME}_C_FLAGS)
    set (DETECTED_C_FLAGS "${TRIAL_C_FLAG}")
    break ()
  endif ()
endforeach ()

# N.B. Will not overwrite existing cache variable:
set (${PKG_NAME}_C_FLAGS "${DETECTED_C_FLAGS}"
  CACHE STRING "C compiler flags for ${PKG_DESC}")

find_package_handle_standard_args (
  ${PKG_NAME} DEFAULT_MSG ${PKG_NAME}_C_FLAGS ${PKG_NAME}_C_FLAGS)
mark_as_advanced (${PKG_NAME}_C_FLAGS)
set (${PKG_NAME}_FOUND ${${PKG_NAME}_FOUND} PARENT_SCOPE)

endfunction ()
