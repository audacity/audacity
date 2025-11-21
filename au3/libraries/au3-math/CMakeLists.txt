#[[
A library of mathematical utilities and manipulation of samples
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

addlib( libsoxr            soxr        SOXR        YES   YES   "soxr >= 0.1.1" )

set( SOURCES
   Biquad.cpp
   Biquad.h
   Dither.cpp
   Dither.h
   EBUR128.cpp
   EBUR128.h
   InterpolateAudio.cpp
   InterpolateAudio.h
   LinearFit.h
   Matrix.cpp
   Matrix.h
   PitchName.cpp
   PitchName.h
   Resample.cpp
   Resample.h
   Reverb_libSoX.h
   RoundUpUnsafe.h
   SampleCount.cpp
   SampleCount.h
   SampleFormat.cpp
   SampleFormat.h
   float_cast.h
   Gain.h
)
set( LIBRARIES
   lib-preferences-interface
   PRIVATE
   libsoxr
)
audacity_library( lib-math "${SOURCES}" "${LIBRARIES}"
   "" ""
)
