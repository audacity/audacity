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
   au3-channel-interface
   au3-math-interface
   au3-module-manager-interface
   au3-project-history-interface
)
audacity_library( au3-realtime-effects "${SOURCES}" "${LIBRARIES}"
   "" ""
)
