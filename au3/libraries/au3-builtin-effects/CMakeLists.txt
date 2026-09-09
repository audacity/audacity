#[[
Library of built-in effects. Implement the UI-agnostic APIs of au3-effects.
]]

set( TARGET au3-builtin-effects )

set( SOURCES
#   AmplifyBase.cpp
#   AmplifyBase.h
   AutoDuckBase.cpp
   AutoDuckBase.h
   BassTrebleBase.cpp
   BassTrebleBase.h
   ChangePitchBase.cpp
   ChangePitchBase.h
   ChangeSpeedBase.cpp
   ChangeSpeedBase.h
   ChangeTempoBase.cpp
   ChangeTempoBase.h
#   ClickRemovalBase.cpp
#   ClickRemovalBase.h
   CompressorInstance.cpp
   CompressorInstance.h
   ContrastBase.cpp
   ContrastBase.h
#   DtmfBase.cpp
#   DtmfBase.h
   DistortionBase.cpp
   DistortionBase.h
   EchoBase.cpp
   EchoBase.h
   EqualizationBase.cpp
   EqualizationBase.h
   EqualizationCurves.cpp
   EqualizationCurves.h
   EqualizationCurvesList.cpp
   EqualizationCurvesList.h
   EqualizationFilter.cpp
   EqualizationFilter.h
   EqualizationParameters.cpp
   EqualizationParameters.h
   FindClippingBase.cpp
   FindClippingBase.h
   Generator.cpp
   Generator.h
   LegacyCompressorBase.cpp
   LegacyCompressorBase.h
#   LoudnessBase.cpp
#   LoudnessBase.h
#   NoiseBase.cpp
#   NoiseBase.h
#   NoiseReductionBase.cpp
#   NoiseReductionBase.h
#   NormalizeBase.cpp
#   NormalizeBase.h
#   PaulstretchBase.cpp
#   PaulstretchBase.h
   PhaserBase.cpp
   PhaserBase.h
   PlotSpectrumBase.cpp
   PlotSpectrumBase.h
#   Repair.cpp
#   Repair.h
   RepeatBase.cpp
   RepeatBase.h
#   ReverbBase.cpp
#   ReverbBase.h
   SBSMSBase.cpp
   SBSMSBase.h
   ScienFilterBase.cpp
   ScienFilterBase.h
   SilenceBase.cpp
   SilenceBase.h
   SoundTouchBase.cpp
   SoundTouchBase.h
#   StereoToMono.cpp # need some cleanups, commenting for now
#   StereoToMono.h # need some cleanups, commenting for now
   ToneGenBase.cpp
   ToneGenBase.h
   TruncSilenceBase.cpp
   TruncSilenceBase.h
   TwoPassSimpleMono.cpp
   TwoPassSimpleMono.h
   WahWahBase.cpp
   WahWahBase.h
)
set( LIBRARIES
PUBLIC
   au3-command-parameters
   au3-components
   au3-dynamic-range-processor
   au3-effects
   au3-fft
   au3-math
   au3-mixer
   au3-strings
   au3-track
   au3-utility
   au3-wave-track
   au3-xml
   # SBSMSBase.h includes <sbsms.h>; making it PRIVATE again is tracked in
   # https://github.com/audacity/audacity/issues/11803
   $<$<BOOL:${AU_USE_SBSMS}>:libsbsms>
PRIVATE
   au3-basic-ui
   au3-exceptions
   au3-files
   au3-label-track
   au3-module-manager
#   au3-note-track # don't link for now as we need USE_MIDI
   au3-numeric-formats
   au3-preferences
   au3-project
   au3-project-rate
   au3-realtime-effects
   au3-time-frequency-selection
   au3-wave-track-fft
   $<$<BOOL:${AU_USE_SOUNDTOUCH}>:soundtouch>
   wxBase
)
set( DEFINES
   PUBLIC
   $<$<BOOL:${AU_USE_SOUNDTOUCH}>:USE_SOUNDTOUCH=1>
   $<$<BOOL:${AU_USE_SBSMS}>:USE_SBSMS=1>
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "${DEFINES}" ""
)
