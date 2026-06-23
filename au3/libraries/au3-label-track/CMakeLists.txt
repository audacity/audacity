#[[
Library for label track.
]]

set( TARGET au3-label-track )

set( SOURCES
   AnalysisTracks.cpp
   AnalysisTracks.h
   LabelTrack.cpp
   LabelTrack.h
)
set( LIBRARIES
   au3-effects-interface
   au3-strings-interface
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
