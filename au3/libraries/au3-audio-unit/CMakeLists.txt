#[[
AudioUnit effect processing logic, minus user interface
]]

set( TARGET au3-audio-unit )

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
PUBLIC
   au3-components
   au3-effects
   au3-strings
   au3-utility
PRIVATE
   au3-basic-ui
   au3-exceptions
   au3-module-manager
   "-framework AudioUnit"
   "-framework AudioToolbox"
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)

