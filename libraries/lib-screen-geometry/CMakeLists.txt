#[[
Classes relating to the mappings between x or y coordinates of the screen, and
times, or frequencies, or amplitudes.
]]

set( SOURCES
   NumberScale.h
   ZoomInfo.cpp
   ZoomInfo.h
)
set( LIBRARIES
)
audacity_library( lib-screen-geometry "${SOURCES}" "${LIBRARIES}"
   "" ""
)
