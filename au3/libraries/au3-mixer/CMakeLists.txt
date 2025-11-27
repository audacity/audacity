#[[
AudioIOSequences defines interfaces to random-access readable, and sequentially
writable, sequences of samples, that are sufficient for the needs of the audio
engine.

SampleTrackCache holds a buffer in memory fetched from a SampleTrack, so
repeated fetches of small but nearby sub-ranges can be satisfied with fewer
but larger fetches from SampleTrack, aligned to underlying database block
boundaries.

Mix combines multiple WideSampleSequences into one output stream of samples,
also handling resampling to different rates.

There is also EffectStage which is used to construct effect processing
pipelines.

Class Envelope represents piecewise linear or exponential curves, and is
sometimes used with an independent variable that is not time.  Perhaps a base
class Curve should be extracted from it and lowered into au3-math.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   AudioIOSequences.cpp
   AudioIOSequences.h
   DownmixSource.cpp
   DownmixSource.h
   DownmixStage.cpp
   DownmixStage.h
   EffectStage.cpp
   EffectStage.h
   Envelope.cpp
   Envelope.h
   Mix.cpp
   Mix.h
   MixerOptions.cpp
   MixerOptions.h
   MixerSource.cpp
   MixerSource.h
   WideSampleSequence.cpp
   WideSampleSequence.h
   WideSampleSource.cpp
   WideSampleSource.h
)
set( LIBRARIES
   au3-audio-graph-interface
   au3-xml-interface
)
audacity_library( au3-mixer "${SOURCES}" "${LIBRARIES}"
   "" ""
)
