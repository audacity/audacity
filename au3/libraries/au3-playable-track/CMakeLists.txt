#[[
Extends Track with subclasses defining the notions of mute and solo.
]]

set( TARGET au3-playable-track )

set( SOURCES
   PlayableTrack.cpp
   PlayableTrack.h
)
set( LIBRARIES
PUBLIC
   au3-preferences
   au3-track
PRIVATE
   au3-strings
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
