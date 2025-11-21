#[[
An audio engine, implemented with the cross-platform portaudio library.

Allows playback and recording and monitoring.

Includes ability to transform the played-back sample streams with effects in
realtime.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
   lib-mixer-interface
   lib-project-rate-interface
   lib-realtime-effects
)
audacity_library( lib-audio-io "${SOURCES}" "${LIBRARIES}"
   "" ""
)
