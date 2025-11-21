#[[
AudioUnit effect processing logic, minus user interface
]]

if (USE_AUDIO_UNITS)

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
   lib-effects-interface
    "-framework AudioUnit"
)
audacity_library( lib-audio-unit "${SOURCES}" "${LIBRARIES}"
   "" ""
)

endif ()
