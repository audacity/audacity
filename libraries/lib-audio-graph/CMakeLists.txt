#[[
Utilities for construction of effect processing pipelines
]]

set( SOURCES
   AudioGraphBuffers.cpp
   AudioGraphBuffers.h
   AudioGraphSink.cpp
   AudioGraphSink.h
   AudioGraphSource.cpp
   AudioGraphSource.h
   AudioGraphTask.cpp
   AudioGraphTask.h
   EffectStage.cpp
   EffectStage.h
)
set( LIBRARIES
   lib-exceptions
   lib-math-interface
   lib-track
   PRIVATE
   wxBase
)
audacity_library( lib-audio-graph "${SOURCES}" "${LIBRARIES}"
   "" ""
)
