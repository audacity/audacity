#[[
Recording, independently of the audio engine: the sink that the device's input
is pushed into and drained from into the recorded sequences, the timing of a
recording, the routing of input channels to track channels, ring buffers between
the audio and worker threads, and the listener
of recording events.
]]

set( TARGET au3-recording )

set( SOURCES
   AudioIOListener.cpp
   AudioIOListener.h
   CaptureRouting.cpp
   CaptureRouting.h
   CaptureSink.cpp
   CaptureSink.h
   RecordingSchedule.cpp
   RecordingSchedule.h
   RingBuffer.cpp
   RingBuffer.h
)
set( LIBRARIES
PUBLIC
   au3-math
   au3-mixer
   au3-utility
PRIVATE
   wxBase
   au3-exceptions
   au3-project
   au3-transactions
   au3-wave-track
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
