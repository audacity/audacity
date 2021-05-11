#ifndef __realfftf_h
#define __realfftf_h





#include "MemoryX.h"

using fft_type = float;
struct FFTParam {
   ArrayOf<int> BitReversed;
   ArrayOf<fft_type> SinTable;
   size_t Points;
#ifdef EXPERIMENTAL_EQ_SSE_THREADED
   int pow2Bits;
#endif
};

struct AUDACITY_DLL_API FFTDeleter{
   void operator () (FFTParam *p) const;
};

using HFFT = std::unique_ptr<
   FFTParam, FFTDeleter
>;

AUDACITY_DLL_API HFFT GetFFT(size_t);
AUDACITY_DLL_API void RealFFTf(fft_type *, const FFTParam *);
AUDACITY_DLL_API void InverseRealFFTf(fft_type *, const FFTParam *);
AUDACITY_DLL_API void ReorderToTime(const FFTParam *hFFT, const fft_type *buffer, fft_type *TimeOut);
AUDACITY_DLL_API void ReorderToFreq(const FFTParam *hFFT, const fft_type *buffer,
		   fft_type *RealOut, fft_type *ImagOut);

#endif

