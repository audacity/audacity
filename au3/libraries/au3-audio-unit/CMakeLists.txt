#[[
AudioUnit effect processing logic, minus user interface
]]

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

