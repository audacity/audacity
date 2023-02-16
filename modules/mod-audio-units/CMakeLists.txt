set( SOURCES
   AUControl.h
   AUControl.mm
   AudioUnitEditor.cpp
   AudioUnitEditor.h
   AudioUnitEffect.cpp
   AudioUnitEffect.h
   AudioUnitEffectOptionsDialog.cpp
   AudioUnitEffectOptionsDialog.h
)
set_source_files_properties(
   effects/audiounits/AUControl.mm
      PROPERTIES
         COMPILE_FLAGS "-x objective-c++"
         SKIP_PRECOMPILE_HEADERS YES
)

set( LIBRARIES
   lib-audio-unit-interface
   lib-wx-effects-interface
)

audacity_module(mod-audio-units "${SOURCES}" "${LIBRARIES}"
   "" ""
)

