#[[
Utilities for construction of effect processing pipelines
]]

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
   au3-math-interface
)
audacity_library( au3-audio-graph "${SOURCES}" "${LIBRARIES}"
   "" ""
)
