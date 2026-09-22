/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_DECIMATORB_H
#define QM_DSP_DECIMATORB_H

#include <vector>

/**
 * DecimatorB carries out a fast downsample by a power-of-two
 * factor. It only knows how to decimate by a factor of 2, and will
 * use repeated decimation for higher factors. A Butterworth filter of
 * order 6 is used for the lowpass filter.
 */
class DecimatorB
{
public:
    void process( const double* src, double* dst );
    void process( const float* src, float* dst );

    /**
     * Construct a DecimatorB to operate on input blocks of length
     * inLength, with decimation factor decFactor.  inLength should be
     * a multiple of decFactor.  Output blocks will be of length
     * inLength / decFactor.
     *
     * decFactor must be a power of two.
     */
    DecimatorB(int inLength, int decFactor);
    virtual ~DecimatorB();

    int getFactor() const { return m_decFactor; }

private:
    void deInitialise();
    void initialise(int inLength, int decFactor);
    void doAntiAlias(const double* src, double* dst, int length, int filteridx);
    void doProcess();

    int m_inputLength;
    int m_outputLength;
    int m_decFactor;

    std::vector<std::vector<double> > m_o;

    double m_a[7];
    double m_b[7];
        
    double *m_aaBuffer;
    double *m_tmpBuffer;
};

#endif

