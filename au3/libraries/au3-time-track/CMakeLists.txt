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
PUBLIC
   au3-track
PRIVATE
   au3-mixer # for Envelope
   au3-project
   au3-project-rate
   au3-strings
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
