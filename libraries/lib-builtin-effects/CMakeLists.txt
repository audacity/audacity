#[[
Library of built-in effects. Implement the UI-agnostic APIs of lib-effects.
]]

set( SOURCES
   AmplifyBase.cpp
   AmplifyBase.h
   BassTrebleBase.cpp
   BassTrebleBase.h
   CompressorInstance.cpp
   CompressorInstance.h
   EchoBase.cpp
   EchoBase.h
   Fade.cpp
   Fade.h
   Invert.cpp
   Invert.h
   LoudnessBase.cpp
   LoudnessBase.h
   NormalizeBase.cpp
   NormalizeBase.h
   Repair.cpp
   Repair.h
   Reverse.cpp
   Reverse.h
   StereoToMono.cpp
   StereoToMono.h
   ToneGenBase.cpp
   ToneGenBase.h
)
set( LIBRARIES
   lib-dynamic-range-processor-interface
   lib-label-track-interface
)
audacity_library( lib-builtin-effects "${SOURCES}" "${LIBRARIES}"
   "" ""
)
