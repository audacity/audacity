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

#include <vamp-sdk/FFT.h>

#include <cmath>

#if ( VAMP_SDK_MAJOR_VERSION != 2 || VAMP_SDK_MINOR_VERSION != 5 )
#error Unexpected version of Vamp SDK header included
#endif

#ifdef _MSC_VER
#include <stdlib.h>
#include <malloc.h>
#endif

_VAMP_SDK_PLUGSPACE_BEGIN(FFT.cpp)

namespace Vamp {

#include "FFTimpl.cpp"

void
FFT::forward(unsigned int n,
	     const double *ri, const double *ii,
	     double *ro, double *io)
{
    fft(n, false, ri, ii, ro, io);
}

void
FFT::inverse(unsigned int n,
	     const double *ri, const double *ii,
	     double *ro, double *io)
{
    fft(n, true, ri, ii, ro, io);
}

}

_VAMP_SDK_PLUGSPACE_END(FFT.cpp)

