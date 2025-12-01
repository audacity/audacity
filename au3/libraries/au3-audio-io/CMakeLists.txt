#[[
An audio engine, implemented with the cross-platform portaudio library.

Allows playback and recording and monitoring.

Includes ability to transform the played-back sample streams with effects in
realtime.
]]

set( TARGET au3-audio-io )

set( SOURCES
   AudioIO.cpp
   AudioIO.h
   AudioIOExt.cpp
   AudioIOExt.h
   AudioIOListener.cpp
   AudioIOListener.h
   PlaybackSchedule.cpp
   PlaybackSchedule.h
   ProjectAudioIO.cpp
   ProjectAudioIO.h
   RingBuffer.cpp
   RingBuffer.h
)
set( LIBRARIES
   portaudio::portaudio
   au3-mixer-interface
   au3-project-rate-interface
   au3-realtime-effects
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
