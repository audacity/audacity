# SoX Resampler Library       Copyright (c) 2007-16 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${PROJECT_C_FLAGS}")
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${PROJECT_CXX_FLAGS}")
link_libraries (${PROJECT_NAME} ${LIBM_LIBRARIES})

if (${BUILD_EXAMPLES} OR ${BUILD_TESTS})
  set (SOURCES 3-options-input-fn)
  if (${WITH_LSR_BINDINGS})
    set (LSR_SOURCES 1a-lsr)
  endif ()
endif ()

if (${BUILD_EXAMPLES})
  list (APPEND SOURCES 1-single-block 2-stream 4-split-channels)
  if (${WITH_VR32})
    list (APPEND SOURCES 5-variable-rate)
  endif ()
endif ()

foreach (fe ${SOURCES} ${LSR_SOURCES})
  get_filename_component (f ${fe} NAME_WE)
  add_executable (${f} ${fe})
  if (${f} STREQUAL "1a-lsr")
    target_link_libraries (${f} soxr-lsr)
  endif ()
endforeach ()

if (${BUILD_TESTS} AND ${WITH_LSR_BINDINGS})
  add_test (lsr-bindings ${BIN}1a-lsr)
endif ()

file (GLOB INSTALL_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/*.[cCh])
install (FILES ${INSTALL_SOURCES} ${CMAKE_CURRENT_SOURCE_DIR}/README
    DESTINATION ${DOC_INSTALL_DIR}/examples)
