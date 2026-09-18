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
)
set( LIBRARIES
PUBLIC
   au3-preferences
   au3-utility
PRIVATE
   au3-basic-ui
   au3-strings
   portaudio::portaudio
   $<$<BOOL:${AU_USE_PORTMIXER}>:portmixer>
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
