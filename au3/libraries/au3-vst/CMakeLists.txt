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
PUBLIC
   au3-components
   au3-effects
   au3-module-manager
   au3-utility
   au3-xml
PRIVATE
   au3-basic-ui
   au3-files
   au3-strings
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
