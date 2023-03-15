#[[
Framework for destructive application of effects, generators, analyzers
]]

set( SOURCES
   EffectBase.cpp
   EffectBase.h
   Effect.cpp
   Effect.h
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
)
set( LIBRARIES
   lib-audio-io-interface
   lib-command-parameters-interface
   lib-numeric-formats-interface
   lib-wave-track-interface
)
audacity_library( lib-effects "${SOURCES}" "${LIBRARIES}"
   "" ""
)
