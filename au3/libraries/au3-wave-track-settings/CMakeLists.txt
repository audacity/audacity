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
      lib-fft-interface
      lib-wave-track-interface
)
audacity_library( lib-wave-track-settings "${SOURCES}" "${LIBRARIES}"
   "" ""
)
