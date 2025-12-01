#[[
A track that defines a time-warping curve which is applied to the whole project
during playback.  There is meant to be not more than one in a project.
]]

set( TARGET au3-time-track )

set( SOURCES
   TimeTrack.cpp
   TimeTrack.h
)
set( LIBRARIES
   au3-mixer-interface # for Envelope
   au3-project-rate-interface
   au3-track-interface
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
