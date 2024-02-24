#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
A library for fast fourier transforms

Note -- this cannot be combined with lib-math, which depends on libsoxr, which
includes a different version of pffft.h
]]#

set( SOURCES
   FFT.cpp
   FFT.h
   PffftTransformer.h
   PffftTransformer.cpp
   RealFFTf.cpp
   RealFFTf.h
   Spectrum.cpp
   Spectrum.h
)
set( LIBRARIES
   pffft
   lib-strings-interface
   lib-utility-interface
)
audacity_library( lib-fft "${SOURCES}" "${LIBRARIES}"
   "" ""
)
