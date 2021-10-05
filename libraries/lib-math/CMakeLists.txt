#[[
A library of mathematical utilities and manipulation of samples
]]#

addlib( libsoxr            soxr        SOXR        YES   YES   "soxr >= 0.1.1" )

set( SOURCES
   Dither.cpp
   Dither.h
   FFT.cpp
   FFT.h
   InterpolateAudio.cpp
   InterpolateAudio.h
   Matrix.cpp
   Matrix.h
   RealFFTf.cpp
   RealFFTf.h
   Resample.cpp
   Resample.h
   SampleCount.cpp
   SampleCount.h
   SampleFormat.cpp
   SampleFormat.h
   Spectrum.cpp
   Spectrum.h
   float_cast.h
)
set( LIBRARIES
   libsoxr
   lib-preferences-interface
   PRIVATE
   wxBase
)
audacity_library( lib-math "${SOURCES}" "${LIBRARIES}"
   "" ""
)
