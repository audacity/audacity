#[[
Library of built-in effects. Implement the UI-agnostic APIs of lib-effects.
]]

set( SOURCES
   AmplifyBase.cpp
   AmplifyBase.h
   CompressorInstance.cpp
   CompressorInstance.h
   Fade.cpp
   Fade.h
   ToneGenBase.cpp
   ToneGenBase.h
)
set( LIBRARIES
   lib-dynamic-range-processor-interface
   lib-effects-interface
)
audacity_library( lib-builtin-effects "${SOURCES}" "${LIBRARIES}"
   "" ""
)
