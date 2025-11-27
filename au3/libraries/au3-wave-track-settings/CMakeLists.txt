#[[
UI-agnostic library for WaveTrack settings.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   SpectrogramSettings.cpp
   SpectrogramSettings.h
   WaveformSettings.cpp
   WaveformSettings.h
)
set( LIBRARIES
   PUBLIC
      au3-fft-interface
      au3-wave-track-interface
)
audacity_library( au3-wave-track-settings "${SOURCES}" "${LIBRARIES}"
   "" ""
)
