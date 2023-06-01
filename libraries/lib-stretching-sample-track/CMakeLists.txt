set( SOURCES
   AudioSegment.h
   ProjectTempoObserver.cpp
   ProjectTempoObserver.h
   SilenceSegment.cpp
   SilenceSegment.h
   SilenceSegmentProcessor.cpp
   SilenceSegmentProcessor.h
   StretchingPlaybackTrackFactory.cpp
   StretchingPlaybackTrackFactory.h
   StretchingSampleTrack.cpp
   StretchingSampleTrack.h
   WaveClipProcessor.cpp
   WaveClipProcessor.h
   WaveClipSegment.cpp
   WaveClipSegment.h
)
set( LIBRARIES
   lib-audio-io-interface
   lib-numeric-formats-interface
   lib-project-interface
   lib-registries-interface
   lib-sample-track-interface
   lib-time-and-pitch-interface
   lib-wave-track-interface
)
audacity_library( lib-stretching-sample-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
