#[[
Part of the audio engine, specializing in the management of effect stacks
and application of effect transformations to buffers of samples.
]]

set( TARGET au3-realtime-effects )

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
PUBLIC
   au3-components
   au3-registries
   au3-utility
   au3-xml
PRIVATE
   au3-channel
   au3-math
   au3-module-manager
   au3-project
   au3-project-history
   au3-strings
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
