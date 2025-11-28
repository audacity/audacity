#[[
FFT-related operations for WaveTrack
]]

set( SOURCES
   TrackSpectrumTransformer.cpp
   TrackSpectrumTransformer.h
)
set( LIBRARIES
   PUBLIC
      au3-wave-track-interface
      au3-fft-interface
)

audacity_library( au3-wave-track-fft "${SOURCES}" "${LIBRARIES}"
   "" ""
)
