#[[
Part of the audio engine, specializing in the management of effect stacks
and application of effect transformations to buffers of samples.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   RealtimeEffectList.cpp
   RealtimeEffectList.h
   RealtimeEffectManager.cpp
   RealtimeEffectManager.h
   RealtimeEffectState.cpp
   RealtimeEffectState.h
   SavedMasterEffectList.cpp
   SavedMasterEffectList.h
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
