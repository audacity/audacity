# SoX Resampler Library       Copyright (c) 2007-12 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

if (${BUILD_EXAMPLES})
  project (soxr)
  file (GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/*.[cC])
  if (NOT BUILD_SHARED_LIBS AND OPENMP_FOUND)
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_C_FLAGS}")
  endif ()
else ()
  file (GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/3*.c)
endif ()

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${PROJECT_C_FLAGS}")
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${PROJECT_CXX_FLAGS}")
link_libraries (${PROJECT_NAME})

foreach (fe ${SOURCES})
  get_filename_component (f ${fe} NAME_WE)
  add_executable (${f} ${fe})
endforeach ()
