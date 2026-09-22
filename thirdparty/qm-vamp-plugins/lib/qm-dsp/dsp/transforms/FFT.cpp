/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
*/

#include "FFT.h"

#include "maths/MathUtilities.h"

#include "kiss_fft.h"
#include "kiss_fftr.h"

#include <cmath>

#include <iostream>

#include <stdexcept>

class FFT::D
{
public:
    D(int n) : m_n(n) {
        m_planf = kiss_fft_alloc(m_n, 0, NULL, NULL);
        m_plani = kiss_fft_alloc(m_n, 1, NULL, NULL);
        m_kin = new kiss_fft_cpx[m_n];
        m_kout = new kiss_fft_cpx[m_n];
    }

    ~D() {
        kiss_fft_free(m_planf);
        kiss_fft_free(m_plani);
        delete[] m_kin;
        delete[] m_kout;
    }

    void process(bool inverse,
                 const double *ri,
                 const double *ii,
                 double *ro,
                 double *io) {

        for (int i = 0; i < m_n; ++i) {
            m_kin[i].r = ri[i];
            m_kin[i].i = (ii ? ii[i] : 0.0);
        }

        if (!inverse) {

            kiss_fft(m_planf, m_kin, m_kout);

            for (int i = 0; i < m_n; ++i) {
                ro[i] = m_kout[i].r;
                io[i] = m_kout[i].i;
            }

        } else {

            kiss_fft(m_plani, m_kin, m_kout);

            double scale = 1.0 / m_n;

            for (int i = 0; i < m_n; ++i) {
                ro[i] = m_kout[i].r * scale;
                io[i] = m_kout[i].i * scale;
            }
        }
    }
    
private:
    int m_n;
    kiss_fft_cfg m_planf;
    kiss_fft_cfg m_plani;
    kiss_fft_cpx *m_kin;
    kiss_fft_cpx *m_kout;
};        

FFT::FFT(int n) :
    m_d(new D(n))
{
}

FFT::~FFT()
{
    delete m_d;
}

void
FFT::process(bool inverse,
             const double *p_lpRealIn, const double *p_lpImagIn,
             double *p_lpRealOut, double *p_lpImagOut)
{
    m_d->process(inverse,
                 p_lpRealIn, p_lpImagIn,
                 p_lpRealOut, p_lpImagOut);
}
    
class FFTReal::D
{
public:
    D(int n) : m_n(n) {
        if (n % 2) {
            throw std::invalid_argument
                ("nsamples must be even in FFTReal constructor");
        }
        m_planf = kiss_fftr_alloc(m_n, 0, NULL, NULL);
        m_plani = kiss_fftr_alloc(m_n, 1, NULL, NULL);
        m_c = new kiss_fft_cpx[m_n];
    }

    ~D() {
        kiss_fftr_free(m_planf);
        kiss_fftr_free(m_plani);
        delete[] m_c;
    }

    void forward(const double *ri, double *ro, double *io) {

        kiss_fftr(m_planf, ri, m_c);

        for (int i = 0; i <= m_n/2; ++i) {
            ro[i] = m_c[i].r;
            io[i] = m_c[i].i;
        }

        for (int i = 0; i + 1 < m_n/2; ++i) {
            ro[m_n - i - 1] =  ro[i + 1];
            io[m_n - i - 1] = -io[i + 1];
        }
    }

    void forwardMagnitude(const double *ri, double *mo) {

        double *io = new double[m_n];

        forward(ri, mo, io);

        for (int i = 0; i < m_n; ++i) {
            mo[i] = sqrt(mo[i] * mo[i] + io[i] * io[i]);
        }

        delete[] io;
    }

    void inverse(const double *ri, const double *ii, double *ro) {

        // kiss_fftr.h says
        // "input freqdata has nfft/2+1 complex points"

        for (int i = 0; i < m_n/2 + 1; ++i) {
            m_c[i].r = ri[i];
            m_c[i].i = ii[i];
        }
        
        kiss_fftri(m_plani, m_c, ro);

        double scale = 1.0 / m_n;

        for (int i = 0; i < m_n; ++i) {
            ro[i] *= scale;
        }
    }

private:
    int m_n;
    kiss_fftr_cfg m_planf;
    kiss_fftr_cfg m_plani;
    kiss_fft_cpx *m_c;
};

FFTReal::FFTReal(int n) :
    m_d(new D(n)) 
{
}

FFTReal::~FFTReal()
{
    delete m_d;
}

void
FFTReal::forward(const double *ri, double *ro, double *io)
{
    m_d->forward(ri, ro, io);
}

void
FFTReal::forwardMagnitude(const double *ri, double *mo)
{
    m_d->forwardMagnitude(ri, mo);
}

void
FFTReal::inverse(const double *ri, const double *ii, double *ro)
{
    m_d->inverse(ri, ii, ro);
}


    
