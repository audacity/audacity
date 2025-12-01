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
   au3-effects-interface
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
