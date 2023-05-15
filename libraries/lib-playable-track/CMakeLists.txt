#[[
Extends Track with subclasses defining the notions of mute and solo.
]]

set( SOURCES
   PlayableTrack.cpp
   PlayableTrack.h
)
set( LIBRARIES
   lib-track-interface
)
audacity_library( lib-playable-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
