/**********************************************************************

  FFT3.h -- FFT routines, based on the following but renamed with "3"
  to avoid naming problems. This early implementation from Audacity has
  been upated and the current Audacity version imports wxChar, but if I
  adapt the latest code into the scorealign library, then scorealign
  will depend upon wxWindows. On the other hand, if I don't update to
  the latest, then there are name conflicts when scorealign is used
  within Audacity. For now, at least, I'm just renaming functions, 
  which has no real impact on scorealign (other than function names
  with the mystery "3" appended), but will result in duplicated code
  in Audacity. -RBD

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
  float-to-float conversions, and I added the routines to
  calculate a real FFT and a real power spectrum.

  Note: all of these routines use single-precision floats.
  I have found that in practice, floats work well until you
  get above 8192 samples.  If you need to do a larger FFT,
  you need to use floats.

**********************************************************************/

#ifndef M_PI
#define	M_PI		3.14159265358979323846  /* pi */
#endif

#define false 0
#define true 1

/*
 * This is the function you will use the most often.
 * Given an array of floats, this will compute the power
 * spectrum by doing a Real FFT and then computing the
 * sum of the squares of the real and imaginary parts.
 * Note that the output array is half the length of the
 * input array, and that NumSamples must be a power of two.
 */

void PowerSpectrum3(int NumSamples, float *In, float *Out);

/*
 * Computes an FFT when the input data is real but you still
 * want complex data as output.  The output arrays are half
 * the length of the input, and NumSamples must be a power of
 * two.
 */

void RealFFT3(int NumSamples,
             float *RealIn, float *RealOut, float *ImagOut);

/*
 * Computes a FFT of complex input and returns complex output.
 * Currently this is the only function here that supports the
 * inverse transform as well.
 */

void FFT3(int NumSamples,
         int InverseTransform,
         float *RealIn, float *ImagIn, float *RealOut, float *ImagOut);

/*
 * Applies a windowing function to the data in place
 *
 * 0: Rectangular (no window)
 * 1: Bartlett    (triangular)
 * 2: Hamming
 * 3: Hanning
 */

void WindowFunc3(int whichFunction, int NumSamples, float *data);

/*
 * Returns the name of the windowing function (for UI display)
 */

const char *WindowFuncName3(int whichFunction);

/*
 * Returns the number of windowing functions supported
 */

int NumWindowFuncs3();   
