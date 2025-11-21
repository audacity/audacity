#[[
Library for label track.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   AnalysisTracks.cpp
   AnalysisTracks.h
   LabelTrack.cpp
   LabelTrack.h
   LabelTrackEditing.cpp
)
set( LIBRARIES
   lib-effects-interface
)
audacity_library( lib-label-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
