#[[
A track that defines a time-warping curve which is applied to the whole project
during playback.  There is meant to be not more than one in a project.
]]

set( SOURCES
   TimeTrack.cpp
   TimeTrack.h
)
set( LIBRARIES
   lib-project-rate-interface
   lib-sample-track-interface
)
audacity_library( lib-time-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
