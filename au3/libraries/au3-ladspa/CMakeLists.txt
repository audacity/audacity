#[[
Ladspa effect plugin discovery and processing logic, without user interface
]]

set( TARGET au3-ladspa )

set( SOURCES
   LadspaEffectBase.cpp
   LadspaEffectBase.h
   LadspaEffectsModule.cpp
   LadspaEffectsModule.h
   LadspaInstance.cpp
   LadspaInstance.h
   ladspa.h
)
set( LIBRARIES
PUBLIC
   au3-components
   au3-effects
   au3-utility
PRIVATE
   au3-exceptions
   au3-files
   au3-module-manager
   au3-strings
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
