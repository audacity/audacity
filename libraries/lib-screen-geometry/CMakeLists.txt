#[[
Classes relating to the mappings between x or y coordinates of the screen, and
times, or frequencies, or amplitudes.
]]#

set( SOURCES
   Decibels.cpp
   Decibels.h
   NumberScale.h
   SelectedRegion.cpp
   SelectedRegion.h
   ViewInfo.cpp
   ViewInfo.h
   ZoomInfo.cpp
   ZoomInfo.h
)
set( LIBRARIES
   lib-preferences-interface
   lib-project-interface
   lib-xml-interface
   PRIVATE
   wxBase
)
audacity_library( lib-screen-geometry "${SOURCES}" "${LIBRARIES}"
   "" ""
)
