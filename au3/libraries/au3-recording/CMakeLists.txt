#[[
What recording needs independently of the audio engine: the timing of a
recording, ring buffers between the audio and worker threads, and the listener
of recording events.
]]

set( TARGET au3-recording )

set( SOURCES
   AudioIOListener.cpp
   AudioIOListener.h
   RecordingSchedule.cpp
   RecordingSchedule.h
   RingBuffer.cpp
   RingBuffer.h
)
set( LIBRARIES
PUBLIC
   au3-math
   au3-utility
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
