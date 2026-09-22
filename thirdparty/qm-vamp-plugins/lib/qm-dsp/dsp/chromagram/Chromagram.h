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

#ifndef QM_DSP_CHROMAGRAM_H
#define QM_DSP_CHROMAGRAM_H

#include "dsp/transforms/FFT.h"
#include "base/Window.h"
#include "ConstantQ.h"

struct ChromaConfig {
    double FS;
    double min;
    double max;
    int BPO;
    double CQThresh;
    MathUtilities::NormaliseType normalise;
};

class Chromagram 
{
public: 
    Chromagram( ChromaConfig Config );
    ~Chromagram();

    /**
     * Process a time-domain input signal of length equal to
     * getFrameSize().
     * 
     * The returned buffer contains the chromagram values indexed by
     * bin, with the number of values corresponding to the BPO field
     * in the ChromaConfig supplied at construction. It is owned by
     * the Chromagram object and is reused from one process call to
     * the next.
     */
    double *process(const double *data);
    
    /**
     * Process a frequency-domain input signal generated from a
     * time-domain signal of length equal to getFrameSize() that has
     * been windowed and "fftshifted" to place the zero index in the
     * centre of the frame. The real and imag buffers must each
     * contain the full getFrameSize() frequency bins.
     * 
     * The returned buffer contains the chromagram values indexed by
     * bin, with the number of values corresponding to the BPO field
     * in the ChromaConfig supplied at construction. It is owned by
     * the Chromagram object and is reused from one process call to
     * the next.
     */
    double *process(const double *real, const double *imag);
    
    void unityNormalise(double* src);

    // Complex arithmetic
    double kabs( double real, double imag );
        
    // Results
    int getK() { return m_uK;}
    int getFrameSize() { return m_frameSize; }
    int getHopSize()   { return m_hopSize; }
    
private:
    int initialise( ChromaConfig Config );
    int deInitialise();

    Window<double> *m_window;
    double *m_windowbuf;
        
    double* m_chromadata;
    double m_FMin;
    double m_FMax;
    int m_BPO;
    int m_uK;

    MathUtilities::NormaliseType m_normalise;

    int m_frameSize;
    int m_hopSize;

    FFTReal* m_FFT;
    ConstantQ* m_ConstantQ;

    double* m_FFTRe;
    double* m_FFTIm;
    double* m_CQRe;
    double* m_CQIm;

    bool m_skGenerated;
};

#endif
