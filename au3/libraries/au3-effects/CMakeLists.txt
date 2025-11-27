#[[
Framework for destructive application of effects, generators, analyzers
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   EffectBase.cpp
   EffectBase.h
   Effect.cpp
   Effect.h
   EffectManager.cpp
   EffectManager.h
   EffectOutputTracks.cpp
   EffectOutputTracks.h
   EffectPlugin.cpp
   EffectPlugin.h
   LoadEffects.cpp
   LoadEffects.h
   MixAndRender.cpp
   MixAndRender.h
   PerTrackEffect.cpp
   PerTrackEffect.h
   StatefulEffectBase.cpp
   StatefulEffectBase.h
   StatefulEffect.cpp
   StatefulEffect.h
   StatefulPerTrackEffect.cpp
   StatefulPerTrackEffect.h
)
set( LIBRARIES
   au3-command-parameters-interface
   au3-numeric-formats-interface
   au3-realtime-effects
   au3-stretching-sequence-interface
   au3-wave-track-interface
)
audacity_library( au3-effects "${SOURCES}" "${LIBRARIES}"
   "" ""
)
