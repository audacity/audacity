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

#include "Correlation.h"

#include "MathAliases.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

Correlation::Correlation()
{

}

Correlation::~Correlation()
{

}

void Correlation::doAutoUnBiased(double *src, double *dst, int length)
{
    double tmp = 0.0;
    double outVal = 0.0;

    int i, j;

    for (i = 0; i < length; i++) {
        for (j = i; j < length; j++) {
            tmp += src[ j-i ] * src[ j ]; 
        }

        outVal = tmp / ( length - i );

        if (outVal <= 0) {
            dst[ i ] = EPS;
        } else {
            dst[ i ] = outVal;
        }
        
        tmp = 0.0;
    }
}
