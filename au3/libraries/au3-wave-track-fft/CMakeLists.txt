#[[
FFT-related operations for WaveTrack
]]

set( TARGET au3-wave-track-fft )

set( SOURCES
   TrackSpectrumTransformer.cpp
   TrackSpectrumTransformer.h
)
set( LIBRARIES
   PUBLIC
      au3-wave-track-interface
      au3-fft-interface
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
