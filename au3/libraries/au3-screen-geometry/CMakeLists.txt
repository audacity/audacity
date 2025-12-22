#[[
Classes relating to the mappings between x or y coordinates of the screen, and
times, or frequencies, or amplitudes.
]]

set( TARGET au3-screen-geometry )

set( SOURCES
   ZoomInfo.cpp
   ZoomInfo.h
)
set( LIBRARIES
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
