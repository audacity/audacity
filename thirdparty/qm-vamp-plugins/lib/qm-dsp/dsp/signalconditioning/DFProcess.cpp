/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file 2005-2006 Christian Landone.

    Modifications:

    - delta threshold
    Description: add delta threshold used as offset in the smoothed
    detection function
    Author: Mathieu Barthet
    Date: June 2010

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#include "DFProcess.h"
#include "maths/MathUtilities.h"

#include <cstring>
#include <algorithm>


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

DFProcess::DFProcess( DFProcConfig config )
{
    filtSrc = NULL;
    filtDst = NULL;     
    m_filtScratchIn = NULL;
    m_filtScratchOut = NULL;

    m_FFOrd = 0;

    initialise( config );
}

DFProcess::~DFProcess()
{
    deInitialise();
}

void DFProcess::initialise( DFProcConfig config )
{
    m_length = config.length;
    m_winPre = config.winPre;
    m_winPost = config.winPost;
    m_alphaNormParam = config.AlphaNormParam;

    m_isMedianPositive = config.isMedianPositive;

    filtSrc = new double[ m_length ];
    filtDst = new double[ m_length ];

    Filter::Parameters params;
    params.a = std::vector<double>
        (config.LPACoeffs, config.LPACoeffs + config.LPOrd + 1);
    params.b = std::vector<double>
        (config.LPBCoeffs, config.LPBCoeffs + config.LPOrd + 1);
    
    m_FiltFilt = new FiltFilt(params);
        
    //add delta threshold
    m_delta = config.delta;
}

void DFProcess::deInitialise()
{
    delete [] filtSrc;
    delete [] filtDst;
    delete [] m_filtScratchIn;
    delete [] m_filtScratchOut;
    delete m_FiltFilt;
}

void DFProcess::process(double *src, double* dst)
{
    if (m_length == 0) return;

    removeDCNormalize( src, filtSrc );

    m_FiltFilt->process( filtSrc, filtDst, m_length );

    medianFilter( filtDst, dst );
}


void DFProcess::medianFilter(double *src, double *dst)
{
    int i,k,j,l;
    int index = 0;

    double val = 0;

    double* y = new double[ m_winPost + m_winPre + 1];
    memset( y, 0, sizeof( double ) * ( m_winPost + m_winPre + 1) );

    double* scratch = new double[ m_length ];

    for( i = 0; i < m_winPre; i++) {
        
        if (index >= m_length) {
            break;
        }

        k = i + m_winPost + 1;

        for( j = 0; j < k; j++) {
            y[ j ] = src[ j ];
        }
        scratch[ index ] = MathUtilities::median( y, k );
        index++;
    }

    for(  i = 0; i + m_winPost + m_winPre < m_length; i ++) {
        
        if (index >= m_length) {
            break;
        }
                         
        l = 0;
        for(  j  = i; j < ( i + m_winPost + m_winPre + 1); j++) {
            y[ l ] = src[ j ];
            l++;
        }

        scratch[index] = MathUtilities::median( y, (m_winPost + m_winPre + 1 ));
        index++;
    }

    for( i = std::max( m_length - m_winPost, 1); i < m_length; i++) {
        
        if (index >= m_length) {
            break;
        }

        k = std::max( i - m_winPre, 1);

        l = 0;
        for( j = k; j < m_length; j++) {
            y[ l ] = src[ j ];
            l++;
        }
                
        scratch[index] = MathUtilities::median( y, l);
        index++;
    }

    for( i = 0; i < m_length; i++ ) {
        //add a delta threshold used as an offset when computing the smoothed detection function
        //(helps to discard noise when detecting peaks) 
        val = src[ i ] - scratch[ i ] - m_delta;
                
        if( m_isMedianPositive ) {
            if( val > 0 ) {
                dst[ i ]  = val;
            } else {
                dst[ i ]  = 0;
            }
        } else {
            dst[ i ]  = val;
        }
    }
        
    delete [] y;
    delete [] scratch;
}


void DFProcess::removeDCNormalize( double *src, double*dst )
{
    double DFmax = 0;
    double DFMin = 0;
    double DFAlphaNorm = 0;

    MathUtilities::getFrameMinMax( src, m_length, &DFMin, &DFmax );

    MathUtilities::getAlphaNorm( src, m_length, m_alphaNormParam, &DFAlphaNorm );

    for (int i = 0; i < m_length; i++) {
        dst[ i ] = ( src[ i ] - DFMin ) / DFAlphaNorm; 
    }
}
