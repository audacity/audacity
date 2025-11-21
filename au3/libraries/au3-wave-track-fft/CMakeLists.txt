#[[
FFT-related operations for WaveTrack
]]

set( SOURCES
   TrackSpectrumTransformer.cpp
   TrackSpectrumTransformer.h
)
set( LIBRARIES
   PUBLIC
      lib-wave-track-interface
      lib-fft-interface
)

audacity_library( lib-wave-track-fft "${SOURCES}" "${LIBRARIES}"
   "" ""
)
