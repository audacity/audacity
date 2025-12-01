#[[
VST 2 effect plugin discovery and processing logic, without user interface
]]

set( TARGET au3-vst )

set( SOURCES
   VSTEffectBase.cpp
   VSTEffectBase.h
   VSTEffectsModule.cpp
   VSTEffectsModule.h
   VSTInstance.cpp
   VSTInstance.h
   VSTWrapper.cpp
   VSTWrapper.h
   aeffectx.h
)
set( LIBRARIES
   au3-effects-interface
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
