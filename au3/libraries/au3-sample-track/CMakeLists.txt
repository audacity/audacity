#[[
SampleTrack extends Track with an interface for random access into a range of
stored samples.

WritableSampleTrack extends SampleTrack to support appending.

These interfaces are sufficient for the audio engine to play and record tracks,
so the engine will not depend on a particular realization of sample storage.
]]

set( SOURCES
   SampleTrack.cpp
   SampleTrack.h
)
set( LIBRARIES
   lib-mixer-interface
   lib-playable-track-interface
)
audacity_library( lib-sample-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
