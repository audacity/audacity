#[[
A library of mathematical utilities and manipulation of samples
]]

addlib( libsoxr            soxr        SOXR        YES   YES   "soxr >= 0.1.1" )

set( SOURCES
   Dither.cpp
   Dither.h
   InterpolateAudio.cpp
   InterpolateAudio.h
   LinearFit.h
   Matrix.cpp
   Matrix.h
   Resample.cpp
   Resample.h
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
