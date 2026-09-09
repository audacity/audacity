#[[
Library for label track.
]]

set( TARGET au3-label-track )

set( SOURCES
   AnalysisTracks.cpp
   AnalysisTracks.h
   LabelTrack.cpp
   LabelTrack.h
)
set( LIBRARIES
PUBLIC
   au3-files
   au3-time-frequency-selection
   au3-track
PRIVATE
   au3-basic-ui
   au3-effects
   au3-preferences
   au3-project
   au3-stretching-sequence
   au3-strings
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
