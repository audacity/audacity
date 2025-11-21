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
   lib-track-interface
)
audacity_library( lib-playable-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
