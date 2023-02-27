#[[
A track that holds textual notes associated with time points or intervals.

Import and export of label information.
]]

set( SOURCES
   AnalysisTracks.cpp
   AnalysisTracks.h
   LabelTrack.cpp
   LabelTrack.h
)
set( LIBRARIES
   lib-time-frequency-selection-interface
   lib-track-interface
)
audacity_library( lib-label-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
