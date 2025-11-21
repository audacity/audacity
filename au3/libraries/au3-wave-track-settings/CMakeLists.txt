#[[
UI-agnostic library for WaveTrack settings.
]]

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
