#[[
Library of built-in effects. Implement the UI-agnostic APIs of lib-effects.
]]

set( SOURCES
   AmplifyBase.cpp
   AmplifyBase.h
   BassTrebleBase.cpp
   BassTrebleBase.h
   ChangePitchBase.cpp
   ChangePitchBase.h
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
   ReverbBase.cpp
   ReverbBase.h
   Reverse.cpp
   Reverse.h
   SBSMSBase.cpp
   SBSMSBase.h
   SoundTouchBase.cpp
   SoundTouchBase.h
   StereoToMono.cpp
   StereoToMono.h
   ToneGenBase.cpp
   ToneGenBase.h
)
set( LIBRARIES
   lib-dynamic-range-processor-interface
   lib-fft-interface
   lib-label-track-interface
   lib-note-track-interface
   $<$<BOOL:${USE_SBSMS}>:libsbsms>
   $<$<BOOL:${USE_SOUNDTOUCH}>:soundtouch>
)
audacity_library( lib-builtin-effects "${SOURCES}" "${LIBRARIES}"
   "" ""
)
