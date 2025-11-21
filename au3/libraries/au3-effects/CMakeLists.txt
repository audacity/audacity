#[[
Framework for destructive application of effects, generators, analyzers
]]

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
   lib-command-parameters-interface
   lib-numeric-formats-interface
   lib-realtime-effects
   lib-stretching-sequence-interface
   lib-wave-track-interface
)
audacity_library( lib-effects "${SOURCES}" "${LIBRARIES}"
   "" ""
)
