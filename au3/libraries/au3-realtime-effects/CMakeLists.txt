#[[
Part of the audio engine, specializing in the management of effect stacks
and application of effect transformations to buffers of samples.
]]

set( SOURCES
   RealtimeEffectList.cpp
   RealtimeEffectList.h
   RealtimeEffectManager.cpp
   RealtimeEffectManager.h
   RealtimeEffectState.cpp
   RealtimeEffectState.h
)
set( LIBRARIES
   lib-channel-interface
   lib-math-interface
   lib-module-manager-interface
   lib-project-history-interface
)
audacity_library( lib-realtime-effects "${SOURCES}" "${LIBRARIES}"
   "" ""
)
