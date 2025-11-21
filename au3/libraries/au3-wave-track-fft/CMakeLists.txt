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
      lib-wave-track-interface
      lib-fft-interface
)

audacity_library( lib-wave-track-fft "${SOURCES}" "${LIBRARIES}"
   "" ""
)
