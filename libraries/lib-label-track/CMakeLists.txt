#[[
Library for label track.
]]

set( SOURCES
   LabelTrack.cpp
   LabelTrack.h
   LabelTrackEditing.cpp
)
set( LIBRARIES
   lib-effects-interface
)
audacity_library( lib-label-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
