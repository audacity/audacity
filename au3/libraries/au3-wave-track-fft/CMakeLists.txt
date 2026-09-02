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
   au3-fft
PRIVATE
   au3-strings
   au3-wave-track
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
