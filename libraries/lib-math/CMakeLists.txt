#[[
A library of mathematical utilities and manipulation of samples
]]#

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
   SampleCount.cpp
   SampleCount.h
   SampleFormat.cpp
   SampleFormat.h
   Spectrum.cpp
   Spectrum.h
   float_cast.h
)
set( LIBRARIES
   lib-preferences-interface
   PRIVATE
   wxBase
)
audacity_library( lib-math "${SOURCES}" "${LIBRARIES}"
   "" ""
)
