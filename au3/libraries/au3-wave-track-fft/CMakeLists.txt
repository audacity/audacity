#[[
FFT-related operations for WaveTrack
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
