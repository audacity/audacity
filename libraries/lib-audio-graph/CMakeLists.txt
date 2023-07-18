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
   lib-math-interface
)
audacity_library( lib-audio-graph "${SOURCES}" "${LIBRARIES}"
   "" ""
)
