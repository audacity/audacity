#[[
User interface for effects using the AudioUnit protocol (macOS only)
]]

if( CMAKE_SYSTEM_NAME MATCHES "Darwin" AND USE_AUDIO_UNITS )

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
set( LIBRARIES
   PRIVATE
      Audacity
      lib-audio-unit-interface
)

# Third party libraries linked directly only by this module
# But others may have transitive dependency on them through this
set(EXTRA_CLUSTER_NODES "lib-audio-unit;-framework AudioUnit" PARENT_SCOPE)

audacity_module( mod-audiounit "${SOURCES}" "${LIBRARIES}"
   "" "" )

set_source_files_properties(
   AUControl.mm
   PROPERTIES
      COMPILE_FLAGS "-x objective-c++"
      SKIP_PRECOMPILE_HEADERS YES
)


endif()
