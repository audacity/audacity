/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file 2005-2006 Christian Landone.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#include "ConstantQ.h"
#include "dsp/transforms/FFT.h"
#include "base/Window.h"

#include <iostream>

//----------------------------------------------------------------------------

ConstantQ::ConstantQ( CQConfig config ) :
    m_sparseKernel(0)
{
    initialise(config);
}

ConstantQ::~ConstantQ()
{
    deInitialise();
}

static double squaredModule(const double & xx, const double & yy) {
    return xx*xx + yy*yy;
}

void ConstantQ::sparsekernel()
{
    SparseKernel *sk = new SparseKernel();

    double* windowRe = new double [ m_FFTLength ];
    double* windowIm = new double [ m_FFTLength ];
    double* transfWindowRe = new double [ m_FFTLength ];
    double* transfWindowIm = new double [ m_FFTLength ];
        
    // for each bin value K, calculate temporal kernel, take its fft
    // to calculate the spectral kernel then threshold it to make it
    // sparse and add it to the sparse kernels matrix
    
    double squareThreshold = m_CQThresh * m_CQThresh;

    FFT fft(m_FFTLength);
        
    for (int j = m_uK - 1; j >= 0; --j) {
        
        for (int i = 0; i < m_FFTLength; ++i) {
            windowRe[i] = 0;
            windowIm[i] = 0;
        }

        // Compute a complex sinusoid windowed with a hamming window
        // of the right length
        
        int windowLength = (int)ceil
            (m_dQ * m_FS / (m_FMin * pow(2, (double)j / (double)m_BPO)));

        int origin = m_FFTLength/2 - windowLength/2;

        for (int i = 0; i < windowLength; ++i) {
            double angle = (2.0 * M_PI * m_dQ * i) / windowLength;
            windowRe[origin + i] = cos(angle);
            windowIm[origin + i] = sin(angle);
        }

        // Shape with hamming window
        Window<double> hamming(HammingWindow, windowLength);
        hamming.cut(windowRe + origin);
        hamming.cut(windowIm + origin);

        // Scale
        for (int i = 0; i < windowLength; ++i) {
            windowRe[origin + i] /= windowLength;
        }
        for (int i = 0; i < windowLength; ++i) {
            windowIm[origin + i] /= windowLength;
        }

        // Input is expected to have been fftshifted, so do the
        // same to the input to the fft that contains the kernel
        for (int i = 0; i < m_FFTLength/2; ++i) {
            double temp = windowRe[i];
            windowRe[i] = windowRe[i + m_FFTLength/2];
            windowRe[i + m_FFTLength/2] = temp;
        }
        for (int i = 0; i < m_FFTLength/2; ++i) {
            double temp = windowIm[i];
            windowIm[i] = windowIm[i + m_FFTLength/2];
            windowIm[i + m_FFTLength/2] = temp;
        }
    
        fft.process(false, windowRe, windowIm, transfWindowRe, transfWindowIm);

        // convert to sparse form
        for (int i = 0; i < m_FFTLength; i++) {

            // perform thresholding
            double mag = squaredModule(transfWindowRe[i], transfWindowIm[i]);
            if (mag <= squareThreshold) continue;
                
            // Insert non-zero position indexes
            sk->is.push_back(i);
            sk->js.push_back(j);

            // take conjugate, normalise and add to array for sparse kernel
            sk->real.push_back( transfWindowRe[i] / m_FFTLength);
            sk->imag.push_back(-transfWindowIm[i] / m_FFTLength);
        }
    }

    delete [] windowRe;
    delete [] windowIm;
    delete [] transfWindowRe;
    delete [] transfWindowIm;

    m_sparseKernel = sk;
}

void ConstantQ::initialise( CQConfig Config )
{
    m_FS = Config.FS;             // Sample rate
    m_FMin = Config.min;          // Minimum frequency
    m_FMax = Config.max;          // Maximum frequency
    m_BPO = Config.BPO;           // Bins per octave
    m_CQThresh = Config.CQThresh; // Threshold for sparse kernel generation

    // Q value for filter bank
    m_dQ = 1/(pow(2,(1/(double)m_BPO))-1);

    // No. of constant Q bins
    m_uK = (int)ceil(m_BPO * log(m_FMax/m_FMin)/log(2.0));

    // Length of fft required for this Constant Q filter bank
    m_FFTLength = MathUtilities::nextPowerOfTwo(int(ceil(m_dQ * m_FS/m_FMin)));

    // Hop from one frame to next
    m_hop = m_FFTLength / 8;

    // allocate memory for cqdata
    m_CQdata = new double [2*m_uK];
}

void ConstantQ::deInitialise()
{
    delete [] m_CQdata;
    delete m_sparseKernel;
}

//-----------------------------------------------------------------------------
double* ConstantQ::process( const double* fftdata )
{
    if (!m_sparseKernel) {
        std::cerr << "ERROR: ConstantQ::process: Sparse kernel has not been initialised" << std::endl;
        return m_CQdata;
    }

    SparseKernel *sk = m_sparseKernel;

    for (int row=0; row < 2 * m_uK; row++) {
        m_CQdata[ row ] = 0;
        m_CQdata[ row+1 ] = 0;
    }
    const int *fftbin = &(sk->is[0]);
    const int *cqbin = &(sk->js[0]);
    const double *real = &(sk->real[0]);
    const double *imag = &(sk->imag[0]);
    const int sparseCells = int(sk->real.size());
        
    for (int i = 0; i < sparseCells; i++) {
        const int row = cqbin[i];
        const int col = fftbin[i];
        if (col == 0) continue;
        const double & r1 = real[i];
        const double & i1 = imag[i];
        const double & r2 = fftdata[ (2*m_FFTLength) - 2*col - 2 ];
        const double & i2 = fftdata[ (2*m_FFTLength) - 2*col - 2 + 1 ];
        // add the multiplication
        m_CQdata[ 2*row  ] += (r1*r2 - i1*i2);
        m_CQdata[ 2*row+1] += (r1*i2 + i1*r2);
    }

    return m_CQdata;
}

void ConstantQ::process(const double *FFTRe, const double* FFTIm,
                        double *CQRe, double *CQIm)
{
    if (!m_sparseKernel) {
        std::cerr << "ERROR: ConstantQ::process: Sparse kernel has not been initialised" << std::endl;
        return;
    }

    SparseKernel *sk = m_sparseKernel;

    for (int row = 0; row < m_uK; row++) {
        CQRe[ row ] = 0;
        CQIm[ row ] = 0;
    }

    const int *fftbin = &(sk->is[0]);
    const int *cqbin = &(sk->js[0]);
    const double *real = &(sk->real[0]);
    const double *imag = &(sk->imag[0]);
    const int sparseCells = int(sk->real.size());
        
    for (int i = 0; i<sparseCells; i++) {
        const int row = cqbin[i];
        const int col = fftbin[i];
        if (col == 0) continue;
        const double & r1 = real[i];
        const double & i1 = imag[i];
        const double & r2 = FFTRe[ m_FFTLength - col ];
        const double & i2 = FFTIm[ m_FFTLength - col ];
        // add the multiplication
        CQRe[ row ] += (r1*r2 - i1*i2);
        CQIm[ row ] += (r1*i2 + i1*r2);
    }
}
