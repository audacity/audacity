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

#include "TCSgram.h"

#include <valarray>
#include <cmath>
#include <iostream>
#include <limits>

#include "maths/MathUtilities.h"

TCSGram::TCSGram() :
    m_uNumBins(6)
{
}

TCSGram::~TCSGram()
{
}


void TCSGram::getTCSVector(int iPosition, TCSVector& rTCSVector) const
{
    if (iPosition < 0) {
        rTCSVector = TCSVector();
    } else if (iPosition >= int(m_VectorList.size())) {
        rTCSVector = TCSVector();
    } else {
        rTCSVector = m_VectorList[iPosition].second;
    }
}

long TCSGram::getTime(size_t uPosition) const
{
    return m_VectorList[uPosition].first;
}


void TCSGram::addTCSVector(const TCSVector& rTCSVector)
{
    size_t uSize = m_VectorList.size();
    long lMilliSeconds = static_cast<long>(uSize*m_dFrameDurationMS);
    std::pair<long, TCSVector> p; 
    p.first = lMilliSeconds;
    p.second = rTCSVector;
        
    m_VectorList.push_back(p);
}

long TCSGram::getDuration() const
{
    size_t uSize = m_VectorList.size();
    return static_cast<long>(uSize*m_dFrameDurationMS);
}

void TCSGram::printDebug()
{
    vectorlist_t::iterator vectorIterator = m_VectorList.begin();
        
    while (vectorIterator != m_VectorList.end()) {
        vectorIterator->second.printDebug();
        vectorIterator++;
    }
}
