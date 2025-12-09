#[[
Extends Track with subclasses defining the notions of mute and solo.
]]

set( TARGET au3-playable-track )

set( SOURCES
   PlayableTrack.cpp
   PlayableTrack.h
)
set( LIBRARIES
   au3-track-interface
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
