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

#include "Framer.h"

#include <limits.h>

Framer::Framer() :
    m_sampleLen(0),
    m_framesRead(0),
    m_srcBuffer(0),
    m_dataFrame(0),
    m_strideFrame(0),
    m_frameLength(0),
    m_stepSize(0),
    m_maxFrames(0),
    m_srcIndex(0)
{
}

Framer::~Framer()
{
    delete[] m_dataFrame;
    delete[] m_strideFrame;
}

void Framer::configure(int frameLength, int hop)
{
    m_frameLength = frameLength;
    m_stepSize = hop;

    resetCounters();

    delete[] m_dataFrame;  
    m_dataFrame = new double[ m_frameLength ];

    delete [] m_strideFrame;        
    m_strideFrame = new double[ m_stepSize ];
}

void Framer::getFrame(double *dst)
{
    if ((m_srcIndex + int64_t(m_frameLength)) < m_sampleLen) {

        for (int i = 0; i < m_frameLength; i++) {
            dst[i] = m_srcBuffer[m_srcIndex++]; 
        }
        m_srcIndex -= (m_frameLength - m_stepSize);

    } else { // m_srcIndex is within m_frameLength of m_sampleLen

        int rem = int(m_sampleLen - m_srcIndex);
        int zero = m_frameLength - rem;

        for (int i = 0; i < rem; i++) {
            dst[i] = m_srcBuffer[m_srcIndex++];
        }
                
        for (int i = 0; i < zero; i++ ) {
            dst[rem + i] = 0.0;
        }

        m_srcIndex -= (rem - m_stepSize);
    }

    m_framesRead++;
}

void Framer::resetCounters()
{
    m_framesRead = 0;
    m_srcIndex = 0;
}

int Framer::getMaxNoFrames()
{
    return m_maxFrames;
}

void Framer::setSource(double *src, int64_t length)
{
    m_srcBuffer = src;
    m_sampleLen = length;

    int64_t maxFrames = length / int64_t(m_stepSize);
    if (maxFrames * int64_t(m_stepSize) < length) {
        ++maxFrames;
    }
    if (maxFrames > INT_MAX) maxFrames = INT_MAX;
    m_maxFrames = maxFrames;
}
