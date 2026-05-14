#[[
A library linking PortAudio and sufficient for querying the existing audio
input/output devices and finding their preferred sample rates.

Also a place to store global settings related to the preferred device.

Also abstract class Meter for communicating buffers of samples for display
purposes.

Does not contain an audio engine.
]]

set( TARGET au3-audio-devices )

set( SOURCES
   AudioIOBase.cpp
   AudioIOBase.h
   DeviceChange.cpp
   DeviceChange.h
   DeviceManager.cpp
   DeviceManager.h
   IMeterSender.h
   RtAudioBackend.cpp
   RtAudioBackend.h
)
set( LIBRARIES
   PUBLIC
   rtaudio::rtaudio
   PRIVATE
   au3-preferences-interface
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
