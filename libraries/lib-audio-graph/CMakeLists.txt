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
   lib-math-interface
   lib-track
)
audacity_library( lib-audio-graph "${SOURCES}" "${LIBRARIES}"
   "" ""
)
