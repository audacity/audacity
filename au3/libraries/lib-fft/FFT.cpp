/**********************************************************************

  FFT.cpp

  Dominic Mazzoni

  September 2000

*******************************************************************//*!

\file FFT.cpp
\brief Fast Fourier Transform routines.

  This file contains a few FFT routines, including a real-FFT
  routine that is almost twice as fast as a normal complex FFT,
  and a power spectrum routine when you know you don't care
  about phase information.

  Some of this code was based on a free implementation of an FFT
  by Don Cross, available on the web at:

    http://www.intersrv.com/~dcross/fft.html

  The basic algorithm for his code was based on Numerican Recipes
  in Fortran.  I optimized his code further by reducing array
  accesses, caching the bit reversal table, and eliminating
  float-to-double conversions, and I added the routines to
  calculate a real FFT and a real power spectrum.

*//*******************************************************************/
/*
  Salvo Ventura - November 2006
  Added more window functions:
    * 4: Blackman
    * 5: Blackman-Harris
    * 6: Welch
    * 7: Gaussian(a=2.5)
    * 8: Gaussian(a=3.5)
    * 9: Gaussian(a=4.5)
*/

#include "FFT.h"

#include "Internat.h"
#include "MemoryX.h"

#include <wx/wxcrtvararg.h>
#include <stdlib.h>
#include <math.h>

#include "RealFFTf.h"

using Floats = ArrayOf<float>;
static ArraysOf<int> gFFTBitTable;
static const size_t MaxFastBits = 16;

/* Declare Static functions */
static void InitFFT();

static bool IsPowerOfTwo(size_t x)
{
    if (x < 2) {
        return false;
    }

    if (x & (x - 1)) {          /* Thanks to 'byang' for this cute trick! */
        return false;
    }

    return true;
}

static size_t NumberOfBitsNeeded(size_t PowerOfTwo)
{
    if (PowerOfTwo < 2) {
        wxFprintf(stderr, "Error: FFT called with size %ld\n", PowerOfTwo);
        exit(1);
    }

    size_t i = 0;
    while (PowerOfTwo > 1) {
        PowerOfTwo >>= 1, ++i;
    }

    return i;
}

int ReverseBits(size_t index, size_t NumBits)
{
    size_t i, rev;

    for (i = rev = 0; i < NumBits; i++) {
        rev = (rev << 1) | (index & 1);
        index >>= 1;
    }

    return rev;
}

void InitFFT()
{
    gFFTBitTable.reinit(MaxFastBits);

    size_t len = 2;
    for (size_t b = 1; b <= MaxFastBits; b++) {
        auto& array = gFFTBitTable[b - 1];
        array.reinit(len);
        for (size_t i = 0; i < len; i++) {
            array[i] = ReverseBits(i, b);
        }

        len <<= 1;
    }
}

void DeinitFFT()
{
    gFFTBitTable.reset();
}

static inline size_t FastReverseBits(size_t i, size_t NumBits)
{
    if (NumBits <= MaxFastBits) {
        return gFFTBitTable[NumBits - 1][i];
    } else {
        return ReverseBits(i, NumBits);
    }
}

/*
 * Complex Fast Fourier Transform
 */

void FFT(size_t NumSamples,
         bool InverseTransform,
         const float* RealIn, const float* ImagIn,
         float* RealOut, float* ImagOut)
{
    double angle_numerator = 2.0 * M_PI;
    double tr, ti;               /* temp real, temp imaginary */

    if (!IsPowerOfTwo(NumSamples)) {
        wxFprintf(stderr, "%ld is not a power of two\n", NumSamples);
        exit(1);
    }

    if (!gFFTBitTable) {
        InitFFT();
    }

    if (!InverseTransform) {
        angle_numerator = -angle_numerator;
    }

    /* Number of bits needed to store indices */
    auto NumBits = NumberOfBitsNeeded(NumSamples);

    /*
     **   Do simultaneous data copy and bit-reversal ordering into outputs...
     */

    for (size_t i = 0; i < NumSamples; i++) {
        auto j = FastReverseBits(i, NumBits);
        RealOut[j] = RealIn[i];
        ImagOut[j] = (ImagIn == NULL) ? 0.0 : ImagIn[i];
    }

    /*
     **   Do the FFT itself...
     */

    size_t BlockEnd = 1;
    for (size_t BlockSize = 2; BlockSize <= NumSamples; BlockSize <<= 1) {
        double delta_angle = angle_numerator / (double)BlockSize;

        double sm2 = sin(-2 * delta_angle);
        double sm1 = sin(-delta_angle);
        double cm2 = cos(-2 * delta_angle);
        double cm1 = cos(-delta_angle);
        double w = 2 * cm1;
        double ar0, ar1, ar2, ai0, ai1, ai2;

        for (size_t i = 0; i < NumSamples; i += BlockSize) {
            ar2 = cm2;
            ar1 = cm1;

            ai2 = sm2;
            ai1 = sm1;

            for (size_t j = i, n = 0; n < BlockEnd; j++, n++) {
                ar0 = w * ar1 - ar2;
                ar2 = ar1;
                ar1 = ar0;

                ai0 = w * ai1 - ai2;
                ai2 = ai1;
                ai1 = ai0;

                size_t k = j + BlockEnd;
                tr = ar0 * RealOut[k] - ai0 * ImagOut[k];
                ti = ar0 * ImagOut[k] + ai0 * RealOut[k];

                RealOut[k] = RealOut[j] - tr;
                ImagOut[k] = ImagOut[j] - ti;

                RealOut[j] += tr;
                ImagOut[j] += ti;
            }
        }

        BlockEnd = BlockSize;
    }

    /*
       **   Need to normalize if inverse transform...
     */

    if (InverseTransform) {
        float denom = (float)NumSamples;

        for (size_t i = 0; i < NumSamples; i++) {
            RealOut[i] /= denom;
            ImagOut[i] /= denom;
        }
    }
}

/*
 * Real Fast Fourier Transform
 *
 * This is merely a wrapper of RealFFTf() from RealFFTf.h.
 */

void RealFFT(size_t NumSamples, const float* RealIn, float* RealOut, float* ImagOut)
{
    auto hFFT = GetFFT(NumSamples);
    Floats pFFT{ NumSamples };
    // Copy the data into the processing buffer
    for (size_t i = 0; i < NumSamples; i++) {
        pFFT[i] = RealIn[i];
    }

    // Perform the FFT
    RealFFTf(pFFT.get(), hFFT.get());

    // Copy the data into the real and imaginary outputs
    for (size_t i = 1; i < (NumSamples / 2); i++) {
        RealOut[i]=pFFT[hFFT->BitReversed[i]  ];
        ImagOut[i]=pFFT[hFFT->BitReversed[i] + 1];
    }
    // Handle the (real-only) DC and Fs/2 bins
    RealOut[0] = pFFT[0];
    RealOut[NumSamples / 2] = pFFT[1];
    ImagOut[0] = ImagOut[NumSamples / 2] = 0;
    // Fill in the upper half using symmetry properties
    for (size_t i = NumSamples / 2 + 1; i < NumSamples; i++) {
        RealOut[i] =  RealOut[NumSamples - i];
        ImagOut[i] = -ImagOut[NumSamples - i];
    }
}

/*
 * InverseRealFFT
 *
 * This function computes the inverse of RealFFT, above.
 * The RealIn and ImagIn is assumed to be conjugate-symmetric
 * and as a result the output is purely real.
 * Only the first half of RealIn and ImagIn are used due to this
 * symmetry assumption.
 *
 * This is merely a wrapper of InverseRealFFTf() from RealFFTf.h.
 */
void InverseRealFFT(size_t NumSamples, const float* RealIn, const float* ImagIn,
                    float* RealOut)
{
    auto hFFT = GetFFT(NumSamples);
    Floats pFFT{ NumSamples };
    // Copy the data into the processing buffer
    for (size_t i = 0; i < (NumSamples / 2); i++) {
        pFFT[2 * i  ] = RealIn[i];
    }
    if (ImagIn == NULL) {
        for (size_t i = 0; i < (NumSamples / 2); i++) {
            pFFT[2 * i + 1] = 0;
        }
    } else {
        for (size_t i = 0; i < (NumSamples / 2); i++) {
            pFFT[2 * i + 1] = ImagIn[i];
        }
    }
    // Put the fs/2 component in the imaginary part of the DC bin
    pFFT[1] = RealIn[NumSamples / 2];

    // Perform the FFT
    InverseRealFFTf(pFFT.get(), hFFT.get());

    // Copy the data to the (purely real) output buffer
    ReorderToTime(hFFT.get(), pFFT.get(), RealOut);
}

/*
 * PowerSpectrum
 *
 * This function uses RealFFTf() from RealFFTf.h to perform the real
 * FFT computation, and then squares the real and imaginary part of
 * each coefficient, extracting the power and throwing away the phase.
 *
 * For speed, it does not call RealFFT, but duplicates some
 * of its code.
 */

void PowerSpectrum(size_t NumSamples, const float* In, float* Out)
{
    auto hFFT = GetFFT(NumSamples);
    Floats pFFT{ NumSamples };
    // Copy the data into the processing buffer
    for (size_t i = 0; i < NumSamples; i++) {
        pFFT[i] = In[i];
    }

    // Perform the FFT
    RealFFTf(pFFT.get(), hFFT.get());

    // Copy the data into the real and imaginary outputs
    for (size_t i = 1; i < NumSamples / 2; i++) {
        Out[i]= (pFFT[hFFT->BitReversed[i]  ] * pFFT[hFFT->BitReversed[i]  ])
                + (pFFT[hFFT->BitReversed[i] + 1] * pFFT[hFFT->BitReversed[i] + 1]);
    }
    // Handle the (real-only) DC and Fs/2 bins
    Out[0] = pFFT[0] * pFFT[0];
    Out[NumSamples / 2] = pFFT[1] * pFFT[1];
}

/*
 * Windowing Functions
 */

int NumWindowFuncs()
{
    return eWinFuncCount;
}

const TranslatableString WindowFuncName(int whichFunction)
{
    switch (whichFunction) {
    default:
    case eWinFuncRectangular:
        return XO("Rectangular");
    case eWinFuncBartlett:
        /* i18n-hint a proper name */
        return XO("Bartlett");
    case eWinFuncHamming:
        /* i18n-hint a proper name */
        return XO("Hamming");
    case eWinFuncHann:
        /* i18n-hint a proper name */
        return XO("Hann");
    case eWinFuncBlackman:
        /* i18n-hint a proper name */
        return XO("Blackman");
    case eWinFuncBlackmanHarris:
        /* i18n-hint two proper names */
        return XO("Blackman-Harris");
    case eWinFuncWelch:
        /* i18n-hint a proper name */
        return XO("Welch");
    case eWinFuncGaussian25:
        /* i18n-hint a mathematical function named for C. F. Gauss */
        return XO("Gaussian(a=2.5)");
    case eWinFuncGaussian35:
        /* i18n-hint a mathematical function named for C. F. Gauss */
        return XO("Gaussian(a=3.5)");
    case eWinFuncGaussian45:
        /* i18n-hint a mathematical function named for C. F. Gauss */
        return XO("Gaussian(a=4.5)");
    }
}

void NewWindowFunc(int whichFunction, size_t NumSamplesIn, bool extraSample, float* in)
{
    int NumSamples = (int)NumSamplesIn;
    if (extraSample) {
        wxASSERT(NumSamples > 0);
        --NumSamples;
    }
    wxASSERT(NumSamples > 0);

    switch (whichFunction) {
    default:
        wxFprintf(stderr, "FFT::WindowFunc - Invalid window function: %d\n", whichFunction);
        break;
    case eWinFuncRectangular:
        // Multiply all by 1.0f -- do nothing
        break;

    case eWinFuncBartlett:
    {
        // Bartlett (triangular) window
        const int nPairs = (NumSamples - 1) / 2; // whether even or odd NumSamples, this is correct
        const float denom = NumSamples / 2.0f;
        in[0] = 0.0f;
        for (int ii = 1;
             ii <= nPairs; // Yes, <=
             ++ii) {
            const float value = ii / denom;
            in[ii] *= value;
            in[NumSamples - ii] *= value;
        }
        // When NumSamples is even, in[half] should be multiplied by 1.0, so unchanged
        // When odd, the value of 1.0 is not reached
    }
    break;
    case eWinFuncHamming:
    {
        // Hamming
        const double multiplier = 2 * M_PI / NumSamples;
        static const double coeff0 = 0.54, coeff1 = -0.46;
        for (int ii = 0; ii < NumSamples; ++ii) {
            in[ii] *= coeff0 + coeff1 * cos(ii * multiplier);
        }
    }
    break;
    case eWinFuncHann:
    {
        // Hann
        const double multiplier = 2 * M_PI / NumSamples;
        static const double coeff0 = 0.5, coeff1 = -0.5;
        for (int ii = 0; ii < NumSamples; ++ii) {
            in[ii] *= coeff0 + coeff1 * cos(ii * multiplier);
        }
    }
    break;
    case eWinFuncBlackman:
    {
        // Blackman
        const double multiplier = 2 * M_PI / NumSamples;
        const double multiplier2 = 2 * multiplier;
        static const double coeff0 = 0.42, coeff1 = -0.5, coeff2 = 0.08;
        for (int ii = 0; ii < NumSamples; ++ii) {
            in[ii] *= coeff0 + coeff1 * cos(ii * multiplier) + coeff2 * cos(ii * multiplier2);
        }
    }
    break;
    case eWinFuncBlackmanHarris:
    {
        // Blackman-Harris
        const double multiplier = 2 * M_PI / NumSamples;
        const double multiplier2 = 2 * multiplier;
        const double multiplier3 = 3 * multiplier;
        static const double coeff0 = 0.35875, coeff1 = -0.48829, coeff2 = 0.14128, coeff3 = -0.01168;
        for (int ii = 0; ii < NumSamples; ++ii) {
            in[ii] *= coeff0 + coeff1 * cos(ii * multiplier) + coeff2 * cos(ii * multiplier2) + coeff3 * cos(ii * multiplier3);
        }
    }
    break;
    case eWinFuncWelch:
    {
        // Welch
        const float N = NumSamples;
        for (int ii = 0; ii < NumSamples; ++ii) {
            const float iOverN = ii / N;
            in[ii] *= 4 * iOverN * (1 - iOverN);
        }
    }
    break;
    case eWinFuncGaussian25:
    {
        // Gaussian (a=2.5)
        // Precalculate some values, and simplify the fmla to try and reduce overhead
        static const double A = -2 * 2.5 * 2.5;
        const float N = NumSamples;
        for (int ii = 0; ii < NumSamples; ++ii) {
            const float iOverN = ii / N;
            // full
            // in[ii] *= exp(-0.5*(A*((ii-NumSamples/2)/NumSamples/2))*(A*((ii-NumSamples/2)/NumSamples/2)));
            // reduced
            in[ii] *= exp(A * (0.25 + (iOverN * iOverN) - iOverN));
        }
    }
    break;
    case eWinFuncGaussian35:
    {
        // Gaussian (a=3.5)
        static const double A = -2 * 3.5 * 3.5;
        const float N = NumSamples;
        for (int ii = 0; ii < NumSamples; ++ii) {
            const float iOverN = ii / N;
            in[ii] *= exp(A * (0.25 + (iOverN * iOverN) - iOverN));
        }
    }
    break;
    case eWinFuncGaussian45:
    {
        // Gaussian (a=4.5)
        static const double A = -2 * 4.5 * 4.5;
        const float N = NumSamples;
        for (int ii = 0; ii < NumSamples; ++ii) {
            const float iOverN = ii / N;
            in[ii] *= exp(A * (0.25 + (iOverN * iOverN) - iOverN));
        }
    }
    break;
    }

    if (extraSample && whichFunction != eWinFuncRectangular) {
        double value = 0.0;
        switch (whichFunction) {
        case eWinFuncHamming:
            value = 0.08;
            break;
        case eWinFuncGaussian25:
            value = exp(-2 * 2.5 * 2.5 * 0.25);
            break;
        case eWinFuncGaussian35:
            value = exp(-2 * 3.5 * 3.5 * 0.25);
            break;
        case eWinFuncGaussian45:
            value = exp(-2 * 4.5 * 4.5 * 0.25);
            break;
        default:
            break;
        }
        in[NumSamples] *= value;
    }
}

// See cautions in FFT.h !
void WindowFunc(int whichFunction, size_t NumSamples, float* in)
{
    bool extraSample = false;
    switch (whichFunction) {
    case eWinFuncHamming:
    case eWinFuncHann:
    case eWinFuncBlackman:
    case eWinFuncBlackmanHarris:
        extraSample = true;
        break;
    default:
        break;
    case eWinFuncBartlett:
        // PRL:  Do nothing here either
        // But I want to comment that the old function did this case
        // wrong in the second half of the array, in case NumSamples was odd
        // but I think that never happened, so I am not bothering to preserve that
        break;
    }
    NewWindowFunc(whichFunction, NumSamples, extraSample, in);
}

void DerivativeOfWindowFunc(int whichFunction, size_t NumSamples, bool extraSample, float* in)
{
    if (eWinFuncRectangular == whichFunction) {
        // Rectangular
        // There are deltas at the ends
        wxASSERT(NumSamples > 0);
        --NumSamples;
        // in[0] *= 1.0f;
        for (int ii = 1; ii < (int)NumSamples; ++ii) {
            in[ii] = 0.0f;
        }
        in[NumSamples] *= -1.0f;
        return;
    }

    if (extraSample) {
        wxASSERT(NumSamples > 0);
        --NumSamples;
    }

    wxASSERT(NumSamples > 0);

    double A;
    switch (whichFunction) {
    case eWinFuncBartlett:
    {
        // Bartlett (triangular) window
        // There are discontinuities in the derivative at the ends, and maybe at the midpoint
        const int nPairs = (NumSamples - 1) / 2; // whether even or odd NumSamples, this is correct
        const float value = 2.0f / NumSamples;
        in[0]
            *=// Average the two limiting values of discontinuous derivative
               value / 2.0f;
        for (int ii = 1;
             ii <= nPairs; // Yes, <=
             ++ii) {
            in[ii] *= value;
            in[NumSamples - ii] *= -value;
        }
        if (NumSamples % 2 == 0) {
            // Average the two limiting values of discontinuous derivative
            in[NumSamples / 2] = 0.0f;
        }
        if (extraSample) {
            in[NumSamples]
                *=// Average the two limiting values of discontinuous derivative
                   -value / 2.0f;
        } else {
            // Halve the multiplier previously applied
            // Average the two limiting values of discontinuous derivative
            in[NumSamples - 1] *= 0.5f;
        }
    }
    break;
    case eWinFuncHamming:
    {
        // Hamming
        // There are deltas at the ends
        const double multiplier = 2 * M_PI / NumSamples;
        static const double coeff0 = 0.54, coeff1 = -0.46 * multiplier;
        // TODO This code should be more explicit about the precision it intends.
        // For now we get C4305 warnings, truncation from 'const double' to 'float'
        in[0] *= coeff0;
        if (!extraSample) {
            --NumSamples;
        }
        for (int ii = 0; ii < (int)NumSamples; ++ii) {
            in[ii] *= -coeff1* sin(ii* multiplier);
        }
        if (extraSample) {
            in[NumSamples] *= -coeff0;
        } else {
            // slightly different
            in[NumSamples] *= -coeff0 - coeff1 * sin(NumSamples * multiplier);
        }
    }
    break;
    case eWinFuncHann:
    {
        // Hann
        const double multiplier = 2 * M_PI / NumSamples;
        const double coeff1 = -0.5 * multiplier;
        for (int ii = 0; ii < (int)NumSamples; ++ii) {
            in[ii] *= -coeff1* sin(ii* multiplier);
        }
        if (extraSample) {
            in[NumSamples] = 0.0f;
        }
    }
    break;
    case eWinFuncBlackman:
    {
        // Blackman
        const double multiplier = 2 * M_PI / NumSamples;
        const double multiplier2 = 2 * multiplier;
        const double coeff1 = -0.5 * multiplier, coeff2 = 0.08 * multiplier2;
        for (int ii = 0; ii < (int)NumSamples; ++ii) {
            in[ii] *= -coeff1* sin(ii* multiplier) - coeff2 * sin(ii * multiplier2);
        }
        if (extraSample) {
            in[NumSamples] = 0.0f;
        }
    }
    break;
    case eWinFuncBlackmanHarris:
    {
        // Blackman-Harris
        const double multiplier = 2 * M_PI / NumSamples;
        const double multiplier2 = 2 * multiplier;
        const double multiplier3 = 3 * multiplier;
        const double coeff1 = -0.48829 * multiplier,
                     coeff2 = 0.14128 * multiplier2, coeff3 = -0.01168 * multiplier3;
        for (int ii = 0; ii < (int)NumSamples; ++ii) {
            in[ii] *= -coeff1* sin(ii* multiplier) - coeff2 * sin(ii * multiplier2) - coeff3 * sin(ii * multiplier3);
        }
        if (extraSample) {
            in[NumSamples] = 0.0f;
        }
    }
    break;
    case eWinFuncWelch:
    {
        // Welch
        const float N = NumSamples;
        const float NN = NumSamples * NumSamples;
        for (int ii = 0; ii < (int)NumSamples; ++ii) {
            in[ii] *= 4 * (N - ii - ii) / NN;
        }
        if (extraSample) {
            in[NumSamples] = 0.0f;
        }
        // Average the two limiting values of discontinuous derivative
        in[0] /= 2.0f;
        in[NumSamples - 1] /= 2.0f;
    }
    break;
    case eWinFuncGaussian25:
        // Gaussian (a=2.5)
        A = -2 * 2.5 * 2.5;
        goto Gaussian;
    case eWinFuncGaussian35:
        // Gaussian (a=3.5)
        A = -2 * 3.5 * 3.5;
        goto Gaussian;
    case eWinFuncGaussian45:
        // Gaussian (a=4.5)
        A = -2 * 4.5 * 4.5;
        goto Gaussian;
Gaussian:
        {
            // Gaussian (a=2.5)
            // There are deltas at the ends
            const float invN = 1.0f / NumSamples;
            const float invNN = invN * invN;
            // Simplify formula from the loop for ii == 0, add term for the delta
            in[0] *= exp(A * 0.25) * (1 - invN);
            if (!extraSample) {
                --NumSamples;
            }
            for (int ii = 1; ii < (int)NumSamples; ++ii) {
                const float iOverN = ii * invN;
                in[ii] *= exp(A * (0.25 + (iOverN * iOverN) - iOverN)) * (2 * ii * invNN - invN);
            }
            if (extraSample) {
                in[NumSamples] *= exp(A * 0.25) * (invN - 1);
            } else {
                // Slightly different
                const float iOverN = NumSamples * invN;
                in[NumSamples] *= exp(A * (0.25 + (iOverN * iOverN) - iOverN)) * (2 * NumSamples * invNN - invN - 1);
            }
        }
        break;
    default:
        wxFprintf(stderr, "FFT::DerivativeOfWindowFunc - Invalid window function: %d\n", whichFunction);
    }
}
