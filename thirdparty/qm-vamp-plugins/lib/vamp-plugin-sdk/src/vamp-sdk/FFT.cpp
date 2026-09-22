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

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#if ( VAMP_SDK_MAJOR_VERSION != 2 || VAMP_SDK_MINOR_VERSION != 10 )
#error Unexpected version of Vamp SDK header included
#endif

_VAMP_SDK_PLUGSPACE_BEGIN(FFT.cpp)

#include "FFTimpl.cpp"

namespace Vamp {

using namespace Kiss;

void
FFT::forward(unsigned int un,
	     const double *ri, const double *ii,
	     double *ro, double *io)
{
    int n(un);
    vamp_kiss_fft_cfg c = vamp_kiss_fft_alloc(n, false, 0, 0);
    vamp_kiss_fft_cpx *in = new vamp_kiss_fft_cpx[n];
    vamp_kiss_fft_cpx *out = new vamp_kiss_fft_cpx[n];
    for (int i = 0; i < n; ++i) {
        in[i].r = ri[i];
        in[i].i = 0;
    }
    if (ii) {
        for (int i = 0; i < n; ++i) {
            in[i].i = ii[i];
        }
    }
    vamp_kiss_fft(c, in, out);
    for (int i = 0; i < n; ++i) {
        ro[i] = out[i].r;
        io[i] = out[i].i;
    }
    vamp_kiss_fft_free(c);
    delete[] in;
    delete[] out;
}

void
FFT::inverse(unsigned int un,
	     const double *ri, const double *ii,
	     double *ro, double *io)
{
    int n(un);
    vamp_kiss_fft_cfg c = vamp_kiss_fft_alloc(n, true, 0, 0);
    vamp_kiss_fft_cpx *in = new vamp_kiss_fft_cpx[n];
    vamp_kiss_fft_cpx *out = new vamp_kiss_fft_cpx[n];
    for (int i = 0; i < n; ++i) {
        in[i].r = ri[i];
        in[i].i = 0;
    }
    if (ii) {
        for (int i = 0; i < n; ++i) {
            in[i].i = ii[i];
        }
    }
    vamp_kiss_fft(c, in, out);
    double scale = 1.0 / double(n);
    for (int i = 0; i < n; ++i) {
        ro[i] = out[i].r * scale;
        io[i] = out[i].i * scale;
    }
    vamp_kiss_fft_free(c);
    delete[] in;
    delete[] out;
}

class FFTComplex::D
{
public:
    D(int n) :
        m_n(n),
        m_fconf(vamp_kiss_fft_alloc(n, false, 0, 0)),
        m_iconf(vamp_kiss_fft_alloc(n, true, 0, 0)),
        m_ci(new vamp_kiss_fft_cpx[m_n]),
        m_co(new vamp_kiss_fft_cpx[m_n]) { }

    ~D() {
        vamp_kiss_fftr_free(m_fconf);
        vamp_kiss_fftr_free(m_iconf);
        delete[] m_ci;
        delete[] m_co;
    }

    void forward(const double *ci, double *co) {
        for (int i = 0; i < m_n; ++i) {
            m_ci[i].r = ci[i*2];
            m_ci[i].i = ci[i*2+1];
        }
        vamp_kiss_fft(m_fconf, m_ci, m_co);
        for (int i = 0; i < m_n; ++i) {
            co[i*2] = m_co[i].r;
            co[i*2+1] = m_co[i].i;
        }
    }

    void inverse(const double *ci, double *co) {
        for (int i = 0; i < m_n; ++i) {
            m_ci[i].r = ci[i*2];
            m_ci[i].i = ci[i*2+1];
        }
        vamp_kiss_fft(m_iconf, m_ci, m_co);
        double scale = 1.0 / double(m_n);
        for (int i = 0; i < m_n; ++i) {
            co[i*2] = m_co[i].r * scale;
            co[i*2+1] = m_co[i].i * scale;
        }
    }
    
private:
    int m_n;
    vamp_kiss_fft_cfg m_fconf;
    vamp_kiss_fft_cfg m_iconf;
    vamp_kiss_fft_cpx *m_ci;
    vamp_kiss_fft_cpx *m_co;
};

FFTComplex::FFTComplex(unsigned int n) :
    m_d(new D(n))
{
}

FFTComplex::~FFTComplex()
{
    delete m_d;
}

void
FFTComplex::forward(const double *ci, double *co)
{
    m_d->forward(ci, co);
}

void
FFTComplex::inverse(const double *ci, double *co)
{
    m_d->inverse(ci, co);
}

class FFTReal::D
{
public:
    D(int n) :
        m_n(n),
        m_fconf(vamp_kiss_fftr_alloc(n, false, 0, 0)),
        m_iconf(vamp_kiss_fftr_alloc(n, true, 0, 0)),
        m_ri(new vamp_kiss_fft_scalar[m_n]),
        m_ro(new vamp_kiss_fft_scalar[m_n]),
        m_freq(new vamp_kiss_fft_cpx[n/2+1]) { }

    ~D() {
        vamp_kiss_fftr_free(m_fconf);
        vamp_kiss_fftr_free(m_iconf);
        delete[] m_ri;
        delete[] m_ro;
        delete[] m_freq;
    }

    void forward(const double *ri, double *co) {
        for (int i = 0; i < m_n; ++i) {
            // in case vamp_kiss_fft_scalar is float
            m_ri[i] = ri[i];
        }
        vamp_kiss_fftr(m_fconf, m_ri, m_freq);
        int hs = m_n/2 + 1;
        for (int i = 0; i < hs; ++i) {
            co[i*2] = m_freq[i].r;
            co[i*2+1] = m_freq[i].i;
        }
    }

    void inverse(const double *ci, double *ro) {
        int hs = m_n/2 + 1;
        for (int i = 0; i < hs; ++i) {
            m_freq[i].r = ci[i*2];
            m_freq[i].i = ci[i*2+1];
        }
        vamp_kiss_fftri(m_iconf, m_freq, m_ro);
        double scale = 1.0 / double(m_n);
        for (int i = 0; i < m_n; ++i) {
            ro[i] = m_ro[i] * scale;
        }
    }
    
private:
    int m_n;
    vamp_kiss_fftr_cfg m_fconf;
    vamp_kiss_fftr_cfg m_iconf;
    vamp_kiss_fft_scalar *m_ri;
    vamp_kiss_fft_scalar *m_ro;
    vamp_kiss_fft_cpx *m_freq;
};

FFTReal::FFTReal(unsigned int n) :
    m_d(new D(n))
{
}

FFTReal::~FFTReal()
{
    delete m_d;
}

void
FFTReal::forward(const double *ri, double *co)
{
    m_d->forward(ri, co);
}

void
FFTReal::inverse(const double *ci, double *ro)
{
    m_d->inverse(ci, ro);
}

}

_VAMP_SDK_PLUGSPACE_END(FFT.cpp)

