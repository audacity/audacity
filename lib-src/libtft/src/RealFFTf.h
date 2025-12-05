#ifndef __realfftf_h
#define __realfftf_h

#include "stddef.h"
#include "version.h"
#include <memory>

using namespace std;

using fft_type = TF_DATA_TYPE;
struct FFTParam {
   unique_ptr<int[]> BitReversed;
   unique_ptr<fft_type[]> SinTable;
   size_t Points;
};

unique_ptr<FFTParam> GetFFT(size_t);
void RealFFTf(fft_type *, const FFTParam *);
void InverseRealFFTf(fft_type *, const FFTParam *);
void ReorderToTime(const FFTParam *hFFT, const fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq(const FFTParam *hFFT, const fft_type *buffer,
		   fft_type *RealOut, fft_type *ImagOut);

#endif

