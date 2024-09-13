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
   ChangeTempoBase.cpp
   ChangeTempoBase.h
   ChangeSpeedBase.cpp
   ChangeSpeedBase.h
   CompressorInstance.cpp
   CompressorInstance.h
   ContrastBase.cpp
   ContrastBase.h
   EchoBase.cpp
   EchoBase.h
   Fade.cpp
   Fade.h
   FindClippingBase.cpp
   FindClippingBase.h
   Invert.cpp
   Invert.h
   LegacyCompressorBase.cpp
   LegacyCompressorBase.h
   LoudnessBase.cpp
   LoudnessBase.h
   NormalizeBase.cpp
   NormalizeBase.h
   PaulstretchBase.cpp
   PaulstretchBase.h
   PlotSpectrumBase.cpp
   PlotSpectrumBase.h
   Repair.cpp
   Repair.h
   ReverbBase.cpp
   ReverbBase.h
   Reverse.cpp
   Reverse.h
   SBSMSBase.cpp
   SBSMSBase.h
   ScienFilterBase.cpp
   ScienFilterBase.h
   SoundTouchBase.cpp
   SoundTouchBase.h
   StereoToMono.cpp
   StereoToMono.h
   ToneGenBase.cpp
   ToneGenBase.h
   TwoPassSimpleMono.cpp
   TwoPassSimpleMono.h
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
