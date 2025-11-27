#[[
VST 2 effect plugin discovery and processing logic, without user interface
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
audacity_library( au3-vst "${SOURCES}" "${LIBRARIES}"
   "" ""
)
