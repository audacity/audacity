#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
A library for fast fourier transforms

Note -- this cannot be combined with au3-math, which depends on libsoxr, which
includes a different version of pffft.h
]]

set( TARGET au3-fft )

set( SOURCES
   FFT.cpp
   FFT.h
   PowerSpectrumGetter.cpp
   PowerSpectrumGetter.h
   RealFFTf.cpp
   RealFFTf.h
   Spectrum.cpp
   Spectrum.h
   SpectrumAnalyst.cpp
   SpectrumAnalyst.h
   SpectrumTransformer.cpp
   SpectrumTransformer.h
)
set( LIBRARIES
PUBLIC
   pffft
   au3-math-interface
   au3-strings-interface
   au3-utility-interface
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
