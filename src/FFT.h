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

#ifndef M_PI
#define	M_PI		3.14159265358979323846  /* pi */
#endif

/*
 * This is the function you will use the most often.
 * Given an array of floats, this will compute the power
 * spectrum by doing a Real FFT and then computing the
 * sum of the squares of the real and imaginary parts.
 * Note that the output array is half the length of the
 * input array, and that NumSamples must be a power of two.
 */

void PowerSpectrum(int NumSamples, float *In, float *Out);

/*
 * Computes an FFT when the input data is real but you still
 * want complex data as output.  The output arrays are the
 * same length as the input, but will be conjugate-symmetric
 * NumSamples must be a power of two.
 */

void RealFFT(int NumSamples,
             float *RealIn, float *RealOut, float *ImagOut);

/*
 * Computes an Inverse FFT when the input data is conjugate symmetric
 * so the output is purely real.  NumSamples must be a power of
 * two.
 * Requires: EXPERIMENTAL_USE_REALFFTF
 */
#include "Experimental.h"
#ifdef EXPERIMENTAL_USE_REALFFTF
void InverseRealFFT(int NumSamples,
             float *RealIn, float *ImagIn, float *RealOut);
#endif

/*
 * Computes a FFT of complex input and returns complex output.
 * Currently this is the only function here that supports the
 * inverse transform as well.
 */

void FFT(int NumSamples,
         bool InverseTransform,
         float *RealIn, float *ImagIn, float *RealOut, float *ImagOut);

/*
 * Applies a windowing function to the data in place
 *
 * 0: Rectangular (no window)
 * 1: Bartlett    (triangular)
 * 2: Hamming
 * 3: Hanning
 * 4: Blackman
 * 5: Blackman-Harris
 * 6: Welch
 * 7: Gaussian(a=2.5)
 * 8: Gaussian(a=3.5)
 * 9: Gaussian(a=4.5)
 */

void WindowFunc(int whichFunction, int NumSamples, float *data);

/*
 * Returns the name of the windowing function (for UI display)
 */

const wxChar *WindowFuncName(int whichFunction);

/*
 * Returns the number of windowing functions supported
 */

int NumWindowFuncs();

void DeinitFFT();

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
