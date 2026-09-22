/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file copyright 2006 Martin Gasser.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#include "TonalEstimator.h"

#include <cmath>
#include <iostream>

TonalEstimator::TonalEstimator()
{
    m_Basis.resize(6);

    int i = 0;
        
        
    // circle of fifths
    m_Basis[i].resize(12);
    for (int iP = 0; iP < 12; iP++) {
        m_Basis[i][iP] = std::sin( (7.0 / 6.0) * iP * M_PI);
    }
        
    i++;

    m_Basis[i].resize(12);
    for (int iP = 0; iP < 12; iP++) {
        m_Basis[i][iP] = std::cos( (7.0 / 6.0) * iP * M_PI);
    }
        
    i++;
        
        
    // circle of major thirds
    m_Basis[i].resize(12);
    for (int iP = 0; iP < 12; iP++) {
        m_Basis[i][iP] = 0.6 * std::sin( (2.0 / 3.0) * iP * M_PI);
    }
        
    i++;

    m_Basis[i].resize(12);
    for (int iP = 0; iP < 12; iP++) {
        m_Basis[i][iP] = 0.6 * std::cos( (2.0 / 3.0) * iP * M_PI);
    }

    i++;


    // circle of minor thirds
    m_Basis[i].resize(12);
    for (int iP = 0; iP < 12; iP++) {
        m_Basis[i][iP] = 1.1 * std::sin( (3.0 / 2.0) * iP * M_PI);
    }
        
    i++;

    m_Basis[i].resize(12);
    for (int iP = 0; iP < 12; iP++) {
        m_Basis[i][iP] = 1.1 * std::cos( (3.0 / 2.0) * iP * M_PI);
    }

}

TonalEstimator::~TonalEstimator()
{
}

TCSVector TonalEstimator::transform2TCS(const ChromaVector& rVector)
{
    TCSVector vaRetVal;
    vaRetVal.resize(6, 0.0);
                
    for (int i = 0; i < 6; i++) {
        for (int iP = 0; iP < 12; iP++) {
            vaRetVal[i] += m_Basis[i][iP] * rVector[iP];
        }
    }
        
    return vaRetVal;
}
