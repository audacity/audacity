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

#include <iostream>
#include <cmath>
#include "maths/MathUtilities.h"
#include "Chromagram.h"

//----------------------------------------------------------------------------

Chromagram::Chromagram( ChromaConfig Config ) :
    m_skGenerated(false)
{
    initialise( Config );
}

int Chromagram::initialise( ChromaConfig Config )
{       
    m_FMin = Config.min;                // min freq
    m_FMax = Config.max;                // max freq
    m_BPO  = Config.BPO;                // bins per octave
    m_normalise = Config.normalise;     // if frame normalisation is required

    // Extend range to a full octave
    double octaves = log(m_FMax / m_FMin) / log(2.0);
    m_FMax = m_FMin * pow(2.0, ceil(octaves));

    // Create array for chroma result
    m_chromadata = new double[ m_BPO ];

    // Create Config Structure for ConstantQ operator
    CQConfig ConstantQConfig;

    // Populate CQ config structure with parameters
    // inherited from the Chroma config
    ConstantQConfig.FS = Config.FS;
    ConstantQConfig.min = m_FMin;
    ConstantQConfig.max = m_FMax;
    ConstantQConfig.BPO = m_BPO;
    ConstantQConfig.CQThresh = Config.CQThresh;
        
    // Initialise ConstantQ operator
    m_ConstantQ = new ConstantQ( ConstantQConfig );

    // No. of constant Q bins
    m_uK = m_ConstantQ->getK();

    // Initialise working arrays
    m_frameSize = m_ConstantQ->getFFTLength();
    m_hopSize = m_ConstantQ->getHop();

    // Initialise FFT object    
    m_FFT = new FFTReal(m_frameSize);

    m_FFTRe = new double[ m_frameSize ];
    m_FFTIm = new double[ m_frameSize ];
    m_CQRe  = new double[ m_uK ];
    m_CQIm  = new double[ m_uK ];

    m_window = 0;
    m_windowbuf = 0;

    return 1;
}

Chromagram::~Chromagram()
{
    deInitialise();
}

int Chromagram::deInitialise()
{
    delete[] m_windowbuf;
    delete m_window;

    delete [] m_chromadata;

    delete m_FFT;

    delete m_ConstantQ;

    delete [] m_FFTRe;
    delete [] m_FFTIm;
    delete [] m_CQRe;
    delete [] m_CQIm;
    return 1;
}

//----------------------------------------------------------------------------------
// returns the absolute value of complex number xx + i*yy
double Chromagram::kabs(double xx, double yy)
{
    double ab = sqrt(xx*xx + yy*yy);
    return(ab);
}
//-----------------------------------------------------------------------------------


void Chromagram::unityNormalise(double *src)
{
    double min, max;
    double val = 0;

    MathUtilities::getFrameMinMax( src, m_BPO, & min, &max );

    for (int i = 0; i < m_BPO; i++) {
        val = src[ i ] / max;
        src[ i ] = val;
    }
}


double *Chromagram::process(const double *data)
{
    if (!m_skGenerated) {
        // Generate CQ Kernel 
        m_ConstantQ->sparsekernel();
        m_skGenerated = true;
    }

    if (!m_window) {
        m_window = new Window<double>(HammingWindow, m_frameSize);
        m_windowbuf = new double[m_frameSize];
    }

    for (int i = 0; i < m_frameSize; ++i) {
        m_windowbuf[i] = data[i];
    }
    m_window->cut(m_windowbuf);

    // The frequency-domain version expects pre-fftshifted input - so
    // we must do the same here
    for (int i = 0; i < m_frameSize/2; ++i) {
        double tmp = m_windowbuf[i];
        m_windowbuf[i] = m_windowbuf[i + m_frameSize/2];
        m_windowbuf[i + m_frameSize/2] = tmp;
    }

    m_FFT->forward(m_windowbuf, m_FFTRe, m_FFTIm);

    return process(m_FFTRe, m_FFTIm);
}

double *Chromagram::process(const double *real, const double *imag)
{
    if (!m_skGenerated) {
        // Generate CQ Kernel 
        m_ConstantQ->sparsekernel();
        m_skGenerated = true;
    }

    // initialise chromadata to 0
    for (int i = 0; i < m_BPO; i++) m_chromadata[i] = 0;

    // Calculate ConstantQ frame
    m_ConstantQ->process( real, imag, m_CQRe, m_CQIm );
        
    // add each octave of cq data into Chromagram
    const int octaves = m_uK / m_BPO;
    for (int octave = 0; octave < octaves; octave++) {
        int firstBin = octave*m_BPO;
        for (int i = 0; i < m_BPO; i++) {
            m_chromadata[i] += kabs( m_CQRe[ firstBin + i ],
                                     m_CQIm[ firstBin + i ]);
        }
    }

    MathUtilities::normalise(m_chromadata, m_BPO, m_normalise);

    return m_chromadata;
}


