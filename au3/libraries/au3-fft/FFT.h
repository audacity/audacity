/**********************************************************************

  FFT.h

  Dominic Mazzoni

  September 2000

  This file contains a few FFT routines, including a real-FFT
  routine that is almost twice as fast as a normal complex FFT,
  and a power spectrum routine which is more convenient when
  you know you don't care about phase information.  It now also
  contains a few basic windowing functions.

  Some of this code was based on a free implementation of an FFT
  by Don Cross, available on the web at:

    http://www.intersrv.com/~dcross/fft.html

  The basic algorithm for his code was based on Numerical Recipes
  in Fortran.  I optimized his code further by reducing array
  accesses, caching the bit reversal table, and eliminating
  float-to-double conversions, and I added the routines to
  calculate a real FFT and a real power spectrum.

  Note: all of these routines use single-precision floats.
  I have found that in practice, floats work well until you
  get above 8192 samples.  If you need to do a larger FFT,
  you need to use doubles.

**********************************************************************/
#ifndef __AUDACITY_FFT_H__
#define __AUDACITY_FFT_H__

#include <wx/defs.h>

class TranslatableString;

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

#include <wx/defs.h>
#include <wx/wxchar.h>

#ifndef M_PI
#define     M_PI        3.14159265358979323846  /* pi */
#endif

/*
 * This is the function you will use the most often.
 * Given an array of floats, this will compute the power
 * spectrum by doing a Real FFT and then computing the
 * sum of the squares of the real and imaginary parts.
 * Note that the output array is half the length of the
 * input array, and that NumSamples must be a power of two.
 */

FFT_API
void PowerSpectrum(size_t NumSamples, const float* In, float* Out);

/*
 * Computes an FFT when the input data is real but you still
 * want complex data as output.  The output arrays are the
 * same length as the input, but will be conjugate-symmetric
 * NumSamples must be a power of two.
 */

FFT_API
void RealFFT(size_t NumSamples, const float* RealIn, float* RealOut, float* ImagOut);

/*
 * Computes an Inverse FFT when the input data is conjugate symmetric
 * so the output is purely real.  NumSamples must be a power of
 * two.
 */
FFT_API
void InverseRealFFT(size_t NumSamples, const float* RealIn, const float* ImagIn, float* RealOut);

/*
 * Computes a FFT of complex input and returns complex output.
 * Currently this is the only function here that supports the
 * inverse transform as well.
 */

FFT_API
void FFT(size_t NumSamples, bool InverseTransform, const float* RealIn, const float* ImagIn, float* RealOut, float* ImagOut);

/*
 * Multiply values in data by values of the chosen function
 * DO NOT REUSE!  Prefer NewWindowFunc instead
 * This version was inconsistent whether the window functions were
 * symmetrical about NumSamples / 2, or about (NumSamples - 1) / 2
 * It remains for compatibility until we decide to upgrade all the old uses
 * All functions have 0 in data[0] except Rectangular, Hamming and Gaussians
 */

enum eWindowFunctions : int
{
    eWinFuncRectangular,
    eWinFuncBartlett,
    eWinFuncHamming,
    eWinFuncHann,
    eWinFuncBlackman,
    eWinFuncBlackmanHarris,
    eWinFuncWelch,
    eWinFuncGaussian25,
    eWinFuncGaussian35,
    eWinFuncGaussian45,
    eWinFuncCount
};

FFT_API
void WindowFunc(int whichFunction, size_t NumSamples, float* data);

/*
 * Multiply values in data by values of the chosen function
 * All functions are symmetrical about NumSamples / 2 if extraSample is false,
 * otherwise about (NumSamples - 1) / 2
 * All functions have 0 in data[0] except Rectangular, Hamming and Gaussians
 */
FFT_API
void NewWindowFunc(int whichFunction, size_t NumSamples, bool extraSample, float* data);

/*
 * Multiply values in data by derivative of the chosen function, assuming
 * sampling interval is unit
 * All functions are symmetrical about NumSamples / 2 if extraSample is false,
 * otherwise about (NumSamples - 1) / 2
 * All functions have 0 in data[0] except Rectangular, Hamming and Gaussians
 */
FFT_API
void DerivativeOfWindowFunc(int whichFunction, size_t NumSamples, bool extraSample, float* data);

/*
 * Returns the name of the windowing function (for UI display)
 */

FFT_API const TranslatableString WindowFuncName(int whichFunction);

/*
 * Returns the number of windowing functions supported
 */

FFT_API int NumWindowFuncs();

FFT_API void DeinitFFT();

#endif
