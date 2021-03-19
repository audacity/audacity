#[[
An audio engine, implemented with the cross-platform portaudio library.

Allows playback and recording and monitoring.

Includes ability to transform the played-back sample streams with effects in
realtime.
]]

set( SOURCES
   AudioIO.cpp
   AudioIO.h
   AudioIOExt.cpp
   AudioIOExt.h
   AudioIOListener.h
   PlaybackSchedule.cpp
   PlaybackSchedule.h
   ProjectAudioIO.cpp
   ProjectAudioIO.h
   RealtimeEffectList.cpp
   RealtimeEffectList.h
   RealtimeEffectManager.cpp
   RealtimeEffectManager.h
   RealtimeEffectState.cpp
   RealtimeEffectState.h
   RingBuffer.cpp
   RingBuffer.h
)
set( LIBRARIES
   lib-module-manager-interface
   lib-project-history-interface
   lib-project-rate-interface
   lib-sample-track-interface
   PRIVATE
   wxBase
)
audacity_library( lib-audio-io "${SOURCES}" "${LIBRARIES}"
   "" ""
)
