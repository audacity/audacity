#[[
Utilities for construction of effect processing pipelines
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
