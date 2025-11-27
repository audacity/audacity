#[[
Ladspa effect plugin discovery and processing logic, without user interface
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

if( "${${_OPT}use_ladspa}" )

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
)
audacity_library( au3-ladspa "${SOURCES}" "${LIBRARIES}"
   "" ""
)

endif()
