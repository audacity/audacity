#ifndef __realfftf_h
#define __realfftf_h

#include "Experimental.h"

#define fft_type float
struct FFTParam {
   int *BitReversed;
   fft_type *SinTable;
   size_t Points;
#ifdef EXPERIMENTAL_EQ_SSE_THREADED
   int pow2Bits;
#endif
};
typedef FFTParam * HFFT;

HFFT InitializeFFT(size_t);
void EndFFT(HFFT);
HFFT GetFFT(size_t);
void ReleaseFFT(HFFT);
void CleanupFFT();
void RealFFTf(fft_type *,HFFT);
void InverseRealFFTf(fft_type *,HFFT);
void ReorderToTime(HFFT hFFT, const fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq(HFFT hFFT, const fft_type *buffer,
		   fft_type *RealOut, fft_type *ImagOut);

#endif

