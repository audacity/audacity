#[[
Adds support for AudioUnit plug-ins (macOS only)
]]

if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )

set( SOURCES
   AUControl.h
   AUControl.mm
   AudioUnitEffect.cpp
   AudioUnitEffect.h
   AudioUnitInstance.cpp
   AudioUnitInstance.h
   AudioUnitEffectOptionsDialog.cpp
   AudioUnitEffectOptionsDialog.h
   AudioUnitEffectsModule.cpp
   AudioUnitEffectsModule.h
   AudioUnitUtils.cpp
   AudioUnitUtils.h
   AudioUnitValidator.cpp
   AudioUnitValidator.h
   AudioUnitWrapper.cpp
   AudioUnitWrapper.h
)
set( LIBRARIES
   Audacity
)
audacity_module( mod-audiounits "${SOURCES}" "${LIBRARIES}"
   "" "" )

set_source_files_properties(
   AUControl.mm
   PROPERTIES
      COMPILE_FLAGS "-x objective-c++"
      SKIP_PRECOMPILE_HEADERS YES
)

endif()
