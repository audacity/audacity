#[[
AudioUnit effect processing logic, minus user interface
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   AudioUnitEffectBase.cpp
   AudioUnitEffectBase.h
   AudioUnitEffectsModule.cpp
   AudioUnitEffectsModule.h
   AudioUnitInstance.cpp
   AudioUnitInstance.h
   AudioUnitUtils.cpp
   AudioUnitUtils.h
   AudioUnitWrapper.cpp
   AudioUnitWrapper.h
)
set( LIBRARIES
   au3-utility
   au3-effects-interface
    "-framework AudioUnit"
)
audacity_library( au3-audio-unit "${SOURCES}" "${LIBRARIES}"
   "" ""
)

