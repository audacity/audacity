#[[
Framework for destructive application of effects, generators, analyzers
]]

set( TARGET au3-effects )

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
PUBLIC
   au3-audio-graph
   au3-components
   au3-math
   au3-mixer
   au3-strings
   au3-track
   au3-utility
PRIVATE
   au3-basic-ui
   au3-command-parameters
   au3-exceptions
   au3-files
   au3-module-manager
   au3-numeric-formats
   au3-project
   au3-project-rate
   au3-realtime-effects
   au3-stretching-sequence
   au3-time-frequency-selection
   au3-transactions
   au3-wave-track
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
