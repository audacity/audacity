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
PUBLIC
   au3-audio-devices
   au3-components
   au3-math
   au3-mixer
   au3-registries
   au3-utility
PRIVATE
   au3-basic-ui
   au3-channel
   au3-preferences
   au3-project
   au3-project-rate
   au3-realtime-effects
   au3-strings
   au3-transactions
   au3-wave-track # AudioIO.cpp dynamic_casts to WaveTrack (needs its typeinfo at link)
   portaudio::portaudio
   $<$<BOOL:${AU_USE_PORTMIXER}>:portmixer>
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
