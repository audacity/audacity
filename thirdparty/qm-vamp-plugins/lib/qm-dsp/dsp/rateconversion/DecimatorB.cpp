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

#include "DecimatorB.h"

#include "maths/MathUtilities.h"

#include <iostream>

using std::vector;

DecimatorB::DecimatorB(int inLength, int decFactor)
{
    m_inputLength = 0;
    m_outputLength = 0;
    m_decFactor = 1;
    m_aaBuffer = 0;
    m_tmpBuffer = 0;

    initialise(inLength, decFactor);
}

DecimatorB::~DecimatorB()
{
    deInitialise();
}

void DecimatorB::initialise(int inLength, int decFactor)
{
    m_inputLength = inLength;
    m_decFactor = decFactor;
    m_outputLength = m_inputLength / m_decFactor;

    if (m_decFactor < 2 || !MathUtilities::isPowerOfTwo(m_decFactor)) {
        std::cerr << "ERROR: DecimatorB::initialise: Decimation factor must be a power of 2 and at least 2 (was: " << m_decFactor << ")" << std::endl;
        m_decFactor = 0;
        return;
    }

    if (m_inputLength % m_decFactor != 0) {
        std::cerr << "ERROR: DecimatorB::initialise: inLength must be a multiple of decimation factor (was: " << m_inputLength << ", factor is " << m_decFactor << ")" << std::endl;
        m_decFactor = 0;
        return;
    }        

    m_aaBuffer = new double[m_inputLength];
    m_tmpBuffer = new double[m_inputLength];

    // Order 6 Butterworth lowpass filter
    // Calculated using e.g. MATLAB butter(6, 0.5, 'low')

    m_b[0] = 0.029588223638661;
    m_b[1] = 0.177529341831965;
    m_b[2] = 0.443823354579912;
    m_b[3] = 0.591764472773216;
    m_b[4] = 0.443823354579912;
    m_b[5] = 0.177529341831965;
    m_b[6] = 0.029588223638661;

    m_a[0] = 1.000000000000000;
    m_a[1] = 0.000000000000000;
    m_a[2] = 0.777695961855673;
    m_a[3] = 0.000000000000000;
    m_a[4] = 0.114199425062434;
    m_a[5] = 0.000000000000000;
    m_a[6] = 0.001750925956183;

    for (int factor = m_decFactor; factor > 1; factor /= 2) {
        m_o.push_back(vector<double>(6, 0.0));
    }
}

void DecimatorB::deInitialise()
{
    delete [] m_aaBuffer;
    delete [] m_tmpBuffer;
}

void DecimatorB::doAntiAlias(const double *src, double *dst, int length,
                             int filteridx)
{
    vector<double> &o = m_o[filteridx];

    for (int i = 0; i < length; i++) {

        double input = src[i];
        double output = input * m_b[0] + o[0];

        o[0] = input * m_b[1] - output * m_a[1] + o[1];
        o[1] = input * m_b[2] - output * m_a[2] + o[2];
        o[2] = input * m_b[3] - output * m_a[3] + o[3];
        o[3] = input * m_b[4] - output * m_a[4] + o[4];
        o[4] = input * m_b[5] - output * m_a[5] + o[5];
        o[5] = input * m_b[6] - output * m_a[6];

        dst[i] = output;
    }
}

void DecimatorB::doProcess()
{
    int filteridx = 0;
    int factorDone = 1;

    while (factorDone < m_decFactor) {

        doAntiAlias(m_tmpBuffer, m_aaBuffer,
                    m_inputLength / factorDone,
                    filteridx);

        filteridx ++;
        factorDone *= 2;

        for (int i = 0; i < m_inputLength / factorDone; ++i) {
            m_tmpBuffer[i] = m_aaBuffer[i * 2];
        }
    }
}

void DecimatorB::process(const double *src, double *dst)
{
    if (m_decFactor == 0) return;

    for (int i = 0; i < m_inputLength; ++i) {
        m_tmpBuffer[i] = src[i];
    }

    doProcess();
    
    for (int i = 0; i < m_outputLength; ++i) {
        dst[i] = m_tmpBuffer[i];
    }
}

void DecimatorB::process(const float *src, float *dst)
{
    if (m_decFactor == 0) return;

    for (int i = 0; i < m_inputLength; ++i) {
        m_tmpBuffer[i] = src[i];
    }

    doProcess();
    
    for (int i = 0; i < m_outputLength; ++i) {
        dst[i] = m_tmpBuffer[i];
    }
}
