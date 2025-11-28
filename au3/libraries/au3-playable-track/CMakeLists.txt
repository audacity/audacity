#[[
Extends Track with subclasses defining the notions of mute and solo.
]]

set( SOURCES
   PlayableTrack.cpp
   PlayableTrack.h
)
set( LIBRARIES
   au3-track-interface
)
audacity_library( au3-playable-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
