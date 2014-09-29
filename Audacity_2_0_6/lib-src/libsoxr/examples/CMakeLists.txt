# SoX Resampler Library       Copyright (c) 2007-13 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

if (${BUILD_EXAMPLES})
  project (soxr) # Adds c++ compiler
  file (GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/[1-9]-*.[cC])
elseif (${BUILD_TESTS})
  file (GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/3*.c)
endif ()

if (${BUILD_EXAMPLES} OR ${BUILD_TESTS})
  if (${WITH_LSR_BINDINGS})
    set (LSR_SOURCES 1a-lsr.c)
  endif ()
endif ()

if (NOT BUILD_SHARED_LIBS AND OPENMP_FOUND)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_C_FLAGS}")
endif ()
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${PROJECT_C_FLAGS}")
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${PROJECT_CXX_FLAGS}")
link_libraries (${PROJECT_NAME})

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
install (FILES ${INSTALL_SOURCES} ${CMAKE_CURRENT_SOURCE_DIR}/README DESTINATION ${DOC_INSTALL_DIR}/examples)
