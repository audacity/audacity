# SoX Resampler Library       Copyright (c) 2007-12 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

add_definitions (${PROJECT_DEFS})
link_libraries (${PROJECT_NAME})

file (GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/*.c)
foreach (fe ${SOURCES})
  get_filename_component (f ${fe} NAME_WE)
  add_executable (${f} ${fe})
  if (NOT ${f} MATCHES "^3b")
    set_target_properties(${f} PROPERTIES EXCLUDE_FROM_ALL TRUE)
  endif ()
endforeach ()
