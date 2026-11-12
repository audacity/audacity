#[[
A library linking PortAudio and sufficient for querying the existing audio
input/output devices and finding their preferred sample rates, and for opening
streams on them as the audio settings say.

Also a place to store global settings related to the preferred device.

Also abstract class Meter for communicating buffers of samples for display
purposes, and the hardware mixer of the selected devices.

Does not contain an audio engine.
]]

set( TARGET au3-audio-devices )

set( SOURCES
   AudioDeviceSettings.cpp
   AudioDeviceSettings.h
   DeviceChange.cpp
   DeviceChange.h
   DeviceManager.cpp
   DeviceManager.h
   DeviceMixer.cpp
   DeviceMixer.h
   IMeterSender.h
   PortAudioStream.cpp
   PortAudioStream.h
)
set( LIBRARIES
PUBLIC
   au3-preferences
   au3-utility
   # PortAudioStream.h is PortAudio's API
   portaudio::portaudio
PRIVATE
   $<$<BOOL:${AU_USE_PORTMIXER}>:portmixer>
   au3-basic-ui
   au3-strings
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
