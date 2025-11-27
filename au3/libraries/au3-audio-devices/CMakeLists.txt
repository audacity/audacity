#[[
A library linking PortAudio and sufficient for querying the existing audio
input/output devices and finding their preferred sample rates.

Also a place to store global settings related to the preferred device.

Also abstract class Meter for communicating buffers of samples for display
purposes.

Does not contain an audio engine.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
   portaudio::portaudio
   $<$<BOOL:${USE_PORTMIXER}>:portmixer>
   au3-preferences-interface
)
audacity_library( au3-audio-devices "${SOURCES}" "${LIBRARIES}"
   "" ""
)
