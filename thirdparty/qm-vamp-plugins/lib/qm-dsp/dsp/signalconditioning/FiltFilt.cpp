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

#include "FiltFilt.h"

FiltFilt::FiltFilt(Filter::Parameters parameters) :
    m_filter(parameters)
{
    m_ord = m_filter.getOrder();
}

FiltFilt::~FiltFilt()
{
}

void FiltFilt::process(const double *const QM_R__ src,
                       double *const QM_R__ dst,
                       const int length)
{       
    int i;

    if (length == 0) return;

    int nFilt = m_ord + 1;
    int nFact = 3 * (nFilt - 1);
    int nExt = length + 2 * nFact;

    double *filtScratchIn = new double[ nExt ];
    double *filtScratchOut = new double[ nExt ];
        
    for (i = 0; i < nExt; i++) {
        filtScratchIn[ i ] = 0.0;
        filtScratchOut[ i ] = 0.0;
    }

    // Edge transients reflection
    double sample0 = 2 * src[ 0 ];
    double sampleN = 2 * src[ length - 1 ];

    int index = 0;
    for (i = nFact; i > 0; i--) {
        if (i < length) {
            filtScratchIn[index] = sample0 - src[ i ];
        }
        ++index;
    }
    index = 0;
    for (i = 0; i < nFact; i++) {
        if (i + 1 < length) {
            filtScratchIn[(nExt - nFact) + index] =
                sampleN - src[ (length - 2) - i ];
        }
        ++index;
    }

    for (i = 0; i < length; i++) {
        filtScratchIn[ i + nFact ] = src[ i ];
    }
    
    ////////////////////////////////
    // Do 0Ph filtering
    m_filter.process(filtScratchIn, filtScratchOut, nExt);
        
    // reverse the series for FILTFILT 
    for (i = 0; i < nExt; i++) { 
        filtScratchIn[ i ] = filtScratchOut[ nExt - i - 1];
    }

    // clear filter state
    m_filter.reset();
    
    // do FILTER again 
    m_filter.process(filtScratchIn, filtScratchOut, nExt);

    // reverse the series to output
    for (i = 0; i < length; i++) {
        dst[ i ] = filtScratchOut[ nExt - nFact - i - 1 ];
    }

    delete [] filtScratchIn;
    delete [] filtScratchOut;
}

