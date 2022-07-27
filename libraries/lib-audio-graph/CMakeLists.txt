#[[
Utilities for construction of effect processing pipelines
]]

set( SOURCES
   AudioGraphBuffers.cpp
   AudioGraphBuffers.h
)
set( LIBRARIES
   lib-math-interface
   PRIVATE
   wxBase
)
audacity_library( lib-audio-graph "${SOURCES}" "${LIBRARIES}"
   "" ""
)
