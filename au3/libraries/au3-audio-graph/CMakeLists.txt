#[[
Utilities for construction of effect processing pipelines
]]

set( TARGET au3-audio-graph )

set( SOURCES
   AudioGraphBuffers.cpp
   AudioGraphBuffers.h
   AudioGraphChannel.cpp
   AudioGraphChannel.h
   AudioGraphSink.cpp
   AudioGraphSink.h
   AudioGraphSource.cpp
   AudioGraphSource.h
   AudioGraphTask.cpp
   AudioGraphTask.h
)
set( LIBRARIES
PUBLIC
   au3-math
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
