#[[
A library linking PortAudio and sufficient for querying the existing audio
input/output devices and finding their preferred sample rates.

Also a place to store global settings related to the preferred device.

Also abstract class Meter for communicating buffers of samples for display
purposes.

Does not contain an audio engine.
]]

set( SOURCES
   AudioIOBase.cpp
   AudioIOBase.h
   DeviceChange.cpp
   DeviceChange.h
   DeviceManager.cpp
   DeviceManager.h
   Meter.cpp
   Meter.h
)
set( LIBRARIES
   portaudio::portaudio
   $<$<BOOL:${USE_PORTMIXER}>:portmixer>
   lib-preferences-interface
)
audacity_library( lib-audio-devices "${SOURCES}" "${LIBRARIES}"
   "" ""
)
