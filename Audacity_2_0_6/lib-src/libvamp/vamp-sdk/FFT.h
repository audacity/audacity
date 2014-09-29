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
 * authors.
 * 
 * This class provides double-precision FFTs in power-of-two sizes
 * only. It is slower than more sophisticated library
 * implementations. If these requirements aren't suitable, make other
 * arrangements.
 *
 * The inverse transform is scaled by 1/n.
 *
 * The implementation is from Don Cross's public domain FFT code.
 */
class FFT
{
public:
    /**
     * Calculate a forward transform of size n.
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
     * Calculate an inverse transform of size n.
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

}

_VAMP_SDK_PLUGSPACE_END(FFT.h)

#endif
