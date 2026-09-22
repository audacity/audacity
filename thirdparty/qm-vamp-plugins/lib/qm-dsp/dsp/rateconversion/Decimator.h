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

#ifndef QM_DSP_DECIMATOR_H
#define QM_DSP_DECIMATOR_H

/**
 * Decimator carries out a fast downsample by a power-of-two
 * factor. Only a limited number of factors are supported, from two to
 * whatever getHighestSupportedFactor() returns. This is much faster
 * than Resampler but has a worse signal-noise ratio.
 */
class Decimator  
{
public:
    /**
     * Construct a Decimator to operate on input blocks of length
     * inLength, with decimation factor decFactor.  inLength should be
     * a multiple of decFactor.  Output blocks will be of length
     * inLength / decFactor.
     *
     * decFactor must be a power of two.  The highest supported factor
     * is obtained through getHighestSupportedFactor(); for higher
     * factors, you will need to chain more than one decimator.
     */
    Decimator(int inLength, int decFactor);
    virtual ~Decimator();

    /**
     * Process inLength samples (as supplied to constructor) from src
     * and write inLength / decFactor samples to dst.  Note that src
     * and dst may be the same or overlap (an intermediate buffer is
     * used).
     */
    void process( const double* src, double* dst );

    /**
     * Process inLength samples (as supplied to constructor) from src
     * and write inLength / decFactor samples to dst.  Note that src
     * and dst may be the same or overlap (an intermediate buffer is
     * used).
     */
    void process( const float* src, float* dst );

    int getFactor() const { return m_decFactor; }
    static int getHighestSupportedFactor() { return 8; }

    void resetFilter();

private:
    void deInitialise();
    void initialise( int inLength, int decFactor );
    void doAntiAlias( const double* src, double* dst, int length );
    void doAntiAlias( const float* src, double* dst, int length );

    int m_inputLength;
    int m_outputLength;
    int m_decFactor;

    double Input;
    double Output ;

    double o1,o2,o3,o4,o5,o6,o7;

    double a[ 9 ];
    double b[ 9 ];
        
    double* decBuffer;
};

#endif // 
