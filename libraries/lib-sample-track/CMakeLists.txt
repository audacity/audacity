#[[
SampleTrack extends Track with an interface for random access into a range of
stored samples.

WritableSampleTrack extends SampleTrack to support appending.

Mix combines multiple WideSampleSequences into one output stream of samples,
also handling resampling to different rates.

These interfaces are sufficient for the audio engine to play and record tracks,
so the engine will not depend on a particular realization of sample storage.

There is also EffectStage which is used to construct effect processing
pipelines.
]]

set( SOURCES
   EffectStage.cpp
   EffectStage.h
   Mix.cpp
   Mix.h
   MixerOptions.cpp
   MixerOptions.h
   MixerSource.cpp
   MixerSource.h
   SampleTrack.cpp
   SampleTrack.h
   SampleTrackCache.cpp
   SampleTrackCache.h
   WideSampleSequence.cpp
   WideSampleSequence.h
   WideSampleSource.cpp
   WideSampleSource.h
)
set( LIBRARIES
   lib-audio-graph-interface
   lib-playable-track-interface
)
audacity_library( lib-sample-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
