#[[
Library for label track.
]]

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
audacity_library( au3-label-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
