#[[
A track without content that mixes audio tracks instead of the master, with
mute/solo, effects, volume and pan of its own.
]]

set( TARGET au3-bus-track )

set( SOURCES
   BusTrack.cpp
   BusTrack.h
)
set( LIBRARIES
PUBLIC
   au3-playable-track
PRIVATE
   au3-project
   au3-realtime-effects
   au3-strings
   au3-xml
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
