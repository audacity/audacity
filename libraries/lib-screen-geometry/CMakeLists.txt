#[[
Classes relating to the mappings between x or y coordinates of the screen, and
times, or frequencies, or amplitudes.
]]#

set( SOURCES
   NumberScale.h
   SelectedRegion.cpp
   SelectedRegion.h
   ViewInfo.cpp
   ViewInfo.h
   ZoomInfo.cpp
   ZoomInfo.h
)
set( LIBRARIES
   lib-project-history-interface
)
audacity_library( lib-screen-geometry "${SOURCES}" "${LIBRARIES}"
   "" ""
)
