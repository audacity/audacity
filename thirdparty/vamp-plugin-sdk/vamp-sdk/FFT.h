/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006-2012 Chris Cannam and QMUL.
  
    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR
    ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
    CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

    Except as contained in this notice, the names of the Centre for
    Digital Music; Queen Mary, University of London; and Chris Cannam
    shall not be used in advertising or otherwise to promote the sale,
    use or other dealings in this Software without prior written
    authorization.
*/

#ifndef _VAMP_FFT_H_
#define _VAMP_FFT_H_

#include "plugguard.h"
_VAMP_SDK_PLUGSPACE_BEGIN(FFT.h)

namespace Vamp {

/**
 * A simple FFT implementation provided for convenience of plugin
 * authors. This class provides one-shot (i.e. fixed table state is
 * recalculated every time) double-precision complex-complex
 * transforms. For repeated transforms from real time-domain data, use
 * an FFTComplex or FFTReal object instead.
 *
 * Note: If the SDK has been compiled with the SINGLE_PRECISION_FFT
 * flag, then all FFTs will use single precision internally. The
 * default is double precision. The API uses doubles in either case.
 *
 * The forward transform is unscaled; the inverse transform is scaled
 * by 1/n.
 */
class FFT
{
public:
    /**
     * Calculate a one-shot forward transform of size n.
     * n must be a multiple of 2.
     *
     * ri and ii must point to the real and imaginary component arrays
     * of the input. For real input, ii may be NULL.
     *
     * ro and io must point to enough space to receive the real and
     * imaginary component arrays of the output.
     *
     * All input and output arrays are of size n.
     */
    static void forward(unsigned int n,
                        const double *ri, const double *ii,
                        double *ro, double *io);

    /**
     * Calculate a one-shot inverse transform of size n.
     * n must be a power of 2, greater than 1.
     *
     * ri and ii must point to the real and imaginary component arrays
     * of the input. For real input, ii may be NULL.
     *
     * ro and io must point to enough space to receive the real and
     * imaginary component arrays of the output. The output is scaled
     * by 1/n. The output pointers may not be NULL, even if the output
     * is expected to be real.
     *
     * All input and output arrays are of size n.
     */
    static void inverse(unsigned int n,
                        const double *ri, const double *ii,
                        double *ro, double *io);
};

/**
 * A simple FFT implementation provided for convenience of plugin
 * authors. This class provides double-precision complex-complex
 * transforms.
 *
 * Note: If the SDK has been compiled with the SINGLE_PRECISION_FFT
 * flag, then all FFTs will use single precision internally. The
 * default is double precision. The API uses doubles in either case.
 *
 * The forward transform is unscaled; the inverse transform is scaled
 * by 1/n.
 */
class FFTComplex
{
public:
    /**
     * Prepare to calculate transforms of size n.
     * n must be a multiple of 2.
     */
    FFTComplex(unsigned int n);

    ~FFTComplex();

    /**
     * Calculate a forward transform of size n.
     *
     * ci must point to the interleaved complex input data of size n
     * (that is, 2n doubles in total).
     *
     * co must point to enough space to receive an interleaved complex
     * output array of size n (that is, 2n doubles in total).
     */
    void forward(const double *ci, double *co);

    /**
     * Calculate an inverse transform of size n.
     *
     * ci must point to an interleaved complex input array of size n
     * (that is, 2n doubles in total).
     *
     * co must point to enough space to receive the interleaved
     * complex output data of size n (that is, 2n doubles in
     * total). The output is scaled by 1/n.
     */
    void inverse(const double *ci, double *co);

private:
    class D;
    D *m_d;
};

/**
 * A simple FFT implementation provided for convenience of plugin
 * authors. This class provides transforms between double-precision
 * real time-domain and double-precision complex frequency-domain
 * data.
 *
 * Note: If the SDK has been compiled with the SINGLE_PRECISION_FFT
 * flag, then all FFTs will use single precision internally. The
 * default is double precision. The API uses doubles in either case.
 *
 * The forward transform is unscaled; the inverse transform is scaled
 * by 1/n.
 */
class FFTReal
{
public:
    /**
     * Prepare to calculate transforms of size n.
     * n must be a multiple of 2.
     */
    FFTReal(unsigned int n);

    ~FFTReal();

    /**
     * Calculate a forward transform of size n.
     *
     * ri must point to the real input data of size n.
     *
     * co must point to enough space to receive an interleaved complex
     * output array of size n/2+1 (that is, n+2 doubles in total).
     */
    void forward(const double *ri, double *co);

    /**
     * Calculate an inverse transform of size n.
     *
     * ci must point to an interleaved complex input array of size
     * n/2+1 (that is, n+2 doubles in total).
     *
     * ro must point to enough space to receive the real output data
     * of size n. The output is scaled by 1/n and only the real part
     * is returned.
     */
    void inverse(const double *ci, double *ro);

private:
    class D;
    D *m_d;
};

}

_VAMP_SDK_PLUGSPACE_END(FFT.h)

#endif
