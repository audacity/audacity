#ifndef __realfftf_h
#define __realfftf_h

#define fft_type float
typedef struct FFTParamType {
   int *BitReversed;
   fft_type *SinTable;
   int Points;
} FFTParam;
#define HFFT FFTParam *

HFFT InitializeFFT(int);
void EndFFT(HFFT);
HFFT GetFFT(int);
void ReleaseFFT(HFFT);
void CleanupFFT();
void RealFFTf(fft_type *,HFFT);
void InverseRealFFTf(fft_type *,HFFT);
void ReorderToTime(HFFT hFFT, fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut);

#endif

