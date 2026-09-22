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

#include "Decimator.h"

#include <iostream>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

Decimator::Decimator( int inLength, int decFactor )
{
    m_inputLength = 0;
    m_outputLength = 0;
    m_decFactor = 1;

    initialise( inLength, decFactor );
}

Decimator::~Decimator()
{
    deInitialise();
}

void Decimator::initialise( int inLength, int decFactor)
{
    m_inputLength = inLength;
    m_decFactor = decFactor;
    m_outputLength = m_inputLength / m_decFactor;

    decBuffer = new double[ m_inputLength ];

    // If adding new factors here, add them to
    // getHighestSupportedFactor in the header as well

    if(m_decFactor == 8) {

        //////////////////////////////////////////////////
        b[0] = 0.060111378492136;
        b[1] = -0.257323420830598;
        b[2] = 0.420583503165928;
        b[3] = -0.222750785197418;
        b[4] = -0.222750785197418;
        b[5] = 0.420583503165928;
        b[6] = -0.257323420830598;
        b[7] = 0.060111378492136;

        a[0] = 1;
        a[1] = -5.667654878577432;
        a[2] = 14.062452278088417;
        a[3] = -19.737303840697738;
        a[4] = 16.889698874608641;
        a[5] = -8.796600612325928;
        a[6] = 2.577553446979888;
        a[7] = -0.326903916815751;
        //////////////////////////////////////////////////

    } else if( m_decFactor == 4 ) {
        
        //////////////////////////////////////////////////
        b[ 0 ] = 0.10133306904918619;
        b[ 1 ] = -0.2447523353702363;
        b[ 2 ] = 0.33622528590120965;
        b[ 3 ] = -0.13936581560633518;
        b[ 4 ] = -0.13936581560633382;
        b[ 5 ] = 0.3362252859012087;
        b[ 6 ] = -0.2447523353702358;
        b[ 7 ] = 0.10133306904918594;

        a[ 0 ] = 1;
        a[ 1 ] = -3.9035590278139427;
        a[ 2 ] = 7.5299379980621133;
        a[ 3 ] = -8.6890803793177511;
        a[ 4 ] = 6.4578667096099176;
        a[ 5 ] = -3.0242979431223631;
        a[ 6 ] = 0.83043385136748382;
        a[ 7 ] = -0.094420800837809335;
        //////////////////////////////////////////////////

    } else if( m_decFactor == 2 ) {
        
        //////////////////////////////////////////////////
        b[ 0 ] = 0.20898944260075727;
        b[ 1 ] = 0.40011234879814367;
        b[ 2 ] = 0.819741973072733;
        b[ 3 ] = 1.0087419911682323;
        b[ 4 ] = 1.0087419911682325;
        b[ 5 ] = 0.81974197307273156;
        b[ 6 ] = 0.40011234879814295;
        b[ 7 ] = 0.20898944260075661;

        a[ 0 ] = 1;
        a[ 1 ] = 0.0077331184208358217;
        a[ 2 ] = 1.9853971155964376;
        a[ 3 ] = 0.19296739275341004;
        a[ 4 ] = 1.2330748872852182;
        a[ 5 ] = 0.18705341389316466;
        a[ 6 ] = 0.23659265908013868;
        a[ 7 ] = 0.032352924250533946;

    } else {
        
        if ( m_decFactor != 1 ) {
            std::cerr << "WARNING: Decimator::initialise: unsupported decimation factor " << m_decFactor << ", no antialiasing filter will be used" << std::endl;
        }

        //////////////////////////////////////////////////
        b[ 0 ] = 1;
        b[ 1 ] = 0;
        b[ 2 ] = 0;
        b[ 3 ] = 0;
        b[ 4 ] = 0;
        b[ 5 ] = 0;
        b[ 6 ] = 0;
        b[ 7 ] = 0;

        a[ 0 ] = 1;
        a[ 1 ] = 0;
        a[ 2 ] = 0;
        a[ 3 ] = 0;
        a[ 4 ] = 0;
        a[ 5 ] = 0;
        a[ 6 ] = 0;
        a[ 7 ] = 0;
    }

    resetFilter();
}

void Decimator::deInitialise()
{
    delete [] decBuffer;
}

void Decimator::resetFilter()
{
    Input = Output = 0;

    o1=o2=o3=o4=o5=o6=o7=0;
}

void Decimator::doAntiAlias(const double *src, double *dst, int length)
{
    for (int i = 0; i < length; i++ ) {
        
        Input = (double)src[ i ];

        Output = Input * b[ 0 ] + o1;

        o1 = Input * b[ 1 ] - Output * a[ 1 ] + o2;
        o2 = Input * b[ 2 ] - Output * a[ 2 ] + o3;
        o3 = Input * b[ 3 ] - Output * a[ 3 ] + o4;
        o4 = Input * b[ 4 ] - Output * a[ 4 ] + o5;
        o5 = Input * b[ 5 ] - Output * a[ 5 ] + o6;
        o6 = Input * b[ 6 ] - Output * a[ 6 ] + o7;
        o7 = Input * b[ 7 ] - Output * a[ 7 ] ;

        dst[ i ] = Output;
    }
}

void Decimator::doAntiAlias(const float *src, double *dst, int length)
{
    for (int i = 0; i < length; i++ ) {
        
        Input = (double)src[ i ];

        Output = Input * b[ 0 ] + o1;

        o1 = Input * b[ 1 ] - Output * a[ 1 ] + o2;
        o2 = Input * b[ 2 ] - Output * a[ 2 ] + o3;
        o3 = Input * b[ 3 ] - Output * a[ 3 ] + o4;
        o4 = Input * b[ 4 ] - Output * a[ 4 ] + o5;
        o5 = Input * b[ 5 ] - Output * a[ 5 ] + o6;
        o6 = Input * b[ 6 ] - Output * a[ 6 ] + o7;
        o7 = Input * b[ 7 ] - Output * a[ 7 ] ;

        dst[ i ] = Output;
    }
}

void Decimator::process(const double *src, double *dst)
{
    if (m_decFactor == 1) {
        for (int i = 0; i < m_outputLength; i++ ) {
            dst[i] = src[i];
        }
        return;
    }
        
    doAntiAlias( src, decBuffer, m_inputLength );

    int idx = 0;

    for (int i = 0; i < m_outputLength; i++ ) {
        dst[ idx++ ] = decBuffer[ m_decFactor * i ];
    }
}

void Decimator::process(const float *src, float *dst)
{
    if (m_decFactor == 1) {
        for (int i = 0; i < m_outputLength; i++ ) {
            dst[i] = src[i];
        }
        return;
    }

    doAntiAlias( src, decBuffer, m_inputLength );

    int idx = 0;

    for (int i = 0; i < m_outputLength; i++ ) {
        dst[ idx++ ] = decBuffer[ m_decFactor * i ];
    }
}
