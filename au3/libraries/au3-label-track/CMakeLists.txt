#[[
Library for label track.
]]

set( TARGET au3-label-track )

set( SOURCES
   AnalysisTracks.cpp
   AnalysisTracks.h
   LabelTrack.cpp
   LabelTrack.h
   LabelTrackEditing.cpp
)
set( LIBRARIES
   au3-effects-interface
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
