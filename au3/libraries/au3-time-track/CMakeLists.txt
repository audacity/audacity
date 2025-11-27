#[[
A track that defines a time-warping curve which is applied to the whole project
during playback.  There is meant to be not more than one in a project.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   TimeTrack.cpp
   TimeTrack.h
)
set( LIBRARIES
   au3-mixer-interface # for Envelope
   au3-project-rate-interface
   au3-track-interface
)
audacity_library( au3-time-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
