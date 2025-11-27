#[[
Extends Track with subclasses defining the notions of mute and solo.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
