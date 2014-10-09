#ifndef __realfftf48x_h
#define __realfftf48x_h

#define fft_type float

int SmallRB(int bits, int numberBits);

enum {
   FFT_SinCosBRTable,
   FFT_SinCosTableVBR16,
   FFT_SinCosTableBR16,
   FFT_FastMathBR16,
   FFT_FastMathBR24
};

/* wrapper funcitons. If passed -1 function choice will be made locally */
void RealFFTf1x(fft_type *,HFFT, int functionType=-1);
void InverseRealFFTf1x(fft_type *,HFFT, int functionType=-1);
void ReorderToTime1x(HFFT hFFT, fft_type *buffer, fft_type *TimeOut, int functionType=-1);
void ReorderToFreq1x(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut, int functionType=-1);
void RealFFTf4x(fft_type *,HFFT, int functionType=-1);
void InverseRealFFTf4x(fft_type *,HFFT, int functionType=-1);
void ReorderToTime4x(HFFT hFFT, fft_type *buffer, fft_type *TimeOut, int functionType=-1);
void ReorderToFreq4x(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut, int functionType=-1);

/* SinCosBRTable versions */
void RealFFTf1xSinCosBRTable(fft_type *,HFFT);
void InverseRealFFTf1xSinCosBRTable(fft_type *,HFFT);
void ReorderToTime1xSinCosBRTable(HFFT hFFT, fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq1xSinCosBRTable(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut);
void RealFFTf4xSinCosBRTable(fft_type *,HFFT);
void InverseRealFFTf4xSinCosBRTable(fft_type *,HFFT);
void ReorderToTime4xSinCosBRTable(HFFT hFFT, fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq4xSinCosBRTable(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut);

/* Fast Math BR16 versions */
void RealFFTf1xFastMathBR16(fft_type *,HFFT);
void InverseRealFFTf1xFastMathBR16(fft_type *,HFFT);
void ReorderToTime1xFastMathBR16(HFFT hFFT, fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq1xFastMathBR16(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut);
void RealFFTf4xFastMathBR16(fft_type *,HFFT);
void InverseRealFFTf4xFastMathBR16(fft_type *,HFFT);
void ReorderToTime4xFastMathBR16(HFFT hFFT, fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq4xFastMathBR16(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut);

/* Fast Math BR24 versions */
void RealFFTf1xFastMathBR24(fft_type *,HFFT);
void InverseRealFFTf1xFastMathBR24(fft_type *,HFFT);
void ReorderToTime1xFastMathBR24(HFFT hFFT, fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq1xFastMathBR24(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut);
void RealFFTf4xFastMathBR24(fft_type *,HFFT);
void InverseRealFFTf4xFastMathBR24(fft_type *,HFFT);
void ReorderToTime4xFastMathBR24(HFFT hFFT, fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq4xFastMathBR24(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut);

/* SinCosTable virtual BR versions */
void RealFFTf1xSinCosTableVBR16(fft_type *,HFFT);
void InverseRealFFTf1xSinCosTableVBR16(fft_type *,HFFT);
void ReorderToTime1xSinCosTableVBR16(HFFT hFFT, fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq1xSinCosTableVBR16(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut);
void RealFFTf4xSinCosTableVBR16(fft_type *,HFFT);
void InverseRealFFTf4xSinCosTableVBR16(fft_type *,HFFT);
void ReorderToTime4xSinCosTableVBR16(HFFT hFFT, fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq4xSinCosTableVBR16(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut);

/* SinCosTable BR16 versions */
void RealFFTf1xSinCosTableBR16(fft_type *,HFFT);
void InverseRealFFTf1xSinCosTableBR16(fft_type *,HFFT);
void ReorderToTime1xSinCosTableBR16(HFFT hFFT, fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq1xSinCosTableBR16(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut);
void RealFFTf4xSinCosTableBR16(fft_type *,HFFT);
void InverseRealFFTf4xSinCosTableBR16(fft_type *,HFFT);
void ReorderToTime4xSinCosTableBR16(HFFT hFFT, fft_type *buffer, fft_type *TimeOut);
void ReorderToFreq4xSinCosTableBR16(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut);

void TableUsage(int iMask);

#ifndef M_PI
#define	M_PI		3.14159265358979323846  /* pi */
#endif

typedef struct  {
   float mSin;
   float mCos;
} SinCosStruct;

class SinCosTable {
public:
   int mSinCosTablePow;
   SinCosStruct *mSinCosTable;
   SinCosTable();
   ~SinCosTable(){ delete [] mSinCosTable; };
};

int SmallRB(int bits, int numberBits);
int (*SmallVRB[])(int bits);


#endif

