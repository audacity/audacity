#[[
Classes relating to the mappings between x or y coordinates of the screen, and
times, or frequencies, or amplitudes.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   NumberScale.h
   ZoomInfo.cpp
   ZoomInfo.h
)
set( LIBRARIES
)
audacity_library( au3-screen-geometry "${SOURCES}" "${LIBRARIES}"
   "" ""
)
