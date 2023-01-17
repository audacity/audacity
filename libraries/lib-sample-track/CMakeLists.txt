#[[
SampleTrack extends Track with an interface for random access into a range of
stored samples.

SampleTrackCache holds a buffer in memory fetched from a SampleTrack, so
repeated fetches of small but nearby sub-ranges can be satisfied with fewer
but larger fetches from SampleTrack, aligned to underlying database block
boundaries.

WritableSampleTrack extends SampleTrack to support appending.

Mix combines muliple SampleTracks into one output stream of samples, also
handling resampling to different rates.

These interfaces are sufficient for the audio engine to play and record tracks,
so the engine will not depend on a particular realization of sample storage.
]]

set( SOURCES
   SampleTrack.cpp
   SampleTrack.h
   SampleTrackCache.cpp
   SampleTrackCache.h
   SampleTrackSource.cpp
   SampleTrackSource.h
   Mix.cpp
   Mix.h
   MixerOptions.cpp
   MixerOptions.h
   MixerSource.cpp
   MixerSource.h
)
set( LIBRARIES
   lib-audio-graph-interface
)
audacity_library( lib-sample-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
