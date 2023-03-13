#[[
VST 2 effect plugin discovery and processing logic, without user interface
]]#

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
   lib-effects-interface
)
audacity_library( lib-vst "${SOURCES}" "${LIBRARIES}"
   "" ""
)
