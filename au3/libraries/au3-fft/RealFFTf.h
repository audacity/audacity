#ifndef __realfftf_h
#define __realfftf_h

#include "MemoryX.h"

using fft_type = float;
struct FFTParam {
    ArrayOf<int> BitReversed;
    ArrayOf<fft_type> SinTable;
    size_t Points;
};

struct FFT_API FFTDeleter {
    void operator ()(FFTParam* p) const;
};

using HFFT = std::unique_ptr<
    FFTParam, FFTDeleter
    >;

FFT_API HFFT GetFFT(size_t);
FFT_API void RealFFTf(fft_type*, const FFTParam*);
FFT_API void InverseRealFFTf(fft_type*, const FFTParam*);
FFT_API void ReorderToTime(const FFTParam* hFFT, const fft_type* buffer, fft_type* TimeOut);
FFT_API void ReorderToFreq(const FFTParam* hFFT, const fft_type* buffer, fft_type* RealOut, fft_type* ImagOut);

#endif
