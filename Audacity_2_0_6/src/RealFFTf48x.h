#ifndef __realfftf48x_h
#define __realfftf48x_h

#define fft_type float

HFFT InitializeFFT1x(int);
void EndFFT1x(HFFT);
HFFT GetFFT1x(int);
void ReleaseFFT1x(HFFT);
void CleanupFFT1x();
void RealFFTf1x(fft_type *,HFFT);
void InverseRealFFTf1x(fft_type *,HFFT);
void ReorderToTime1x(HFFT hFFT, fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq1x(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut);
int SmallReverseBits(int bits, int numberBits);
void RealFFTf4x(fft_type *,HFFT);
void InverseRealFFTf4x(fft_type *,HFFT);
void ReorderToTime4x(HFFT hFFT, fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq4x(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut);
void TableUsage(int iMask);

#endif

