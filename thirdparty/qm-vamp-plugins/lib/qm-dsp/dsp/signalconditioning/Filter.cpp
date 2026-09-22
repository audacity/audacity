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

#include "Filter.h"

#include <stdexcept>

using namespace std;

Filter::Filter(Parameters params)
{
    if (params.a.empty()) {
        m_fir = true;
        if (params.b.empty()) {
            throw logic_error("Filter must have at least one pair of coefficients");
        }
    } else {
        m_fir = false;
        if (params.a.size() != params.b.size()) {
            throw logic_error("Inconsistent numbers of filter coefficients");
        }
    }
    
    m_sz = int(params.b.size());
    m_order = m_sz - 1;

    m_a = params.a;
    m_b = params.b;
    
    // We keep some empty space at the start of the buffer, and
    // encroach gradually into it as we add individual sample
    // calculations at the start. Then when we run out of space, we
    // move the buffer back to the end and begin again. This is
    // significantly faster than moving the whole buffer along in
    // 1-sample steps every time.

    m_offmax = 20;
    m_offa = m_offmax;
    m_offb = m_offmax;

    if (!m_fir) {
        m_bufa.resize(m_order + m_offmax);
    }

    m_bufb.resize(m_sz + m_offmax);
}

Filter::~Filter()
{
}

void
Filter::reset()
{
    m_offb = m_offmax;
    m_offa = m_offmax;

    if (!m_fir) {
        m_bufa.assign(m_bufa.size(), 0.0);
    }

    m_bufb.assign(m_bufb.size(), 0.0);
}

void
Filter::process(const double *const QM_R__ in,
                double *const QM_R__ out,
                const int n)
{
    for (int s = 0; s < n; ++s) {

        if (m_offb > 0) {
            --m_offb;
        } else {
            for (int i = m_sz - 2; i >= 0; --i) {
                m_bufb[i + m_offmax + 1] = m_bufb[i];
            }
            m_offb = m_offmax;
        }
        m_bufb[m_offb] = in[s];

        double b_sum = 0.0;
        for (int i = 0; i < m_sz; ++i) {
            b_sum += m_b[i] * m_bufb[i + m_offb];
        }

        double outval;

        if (m_fir) {

            outval = b_sum;

        } else {

            double a_sum = 0.0;
            for (int i = 0; i < m_order; ++i) {
                a_sum += m_a[i + 1] * m_bufa[i + m_offa];
            }

            outval = b_sum - a_sum;

            if (m_offa > 0) {
                --m_offa;
            } else {
                for (int i = m_order - 2; i >= 0; --i) {
                    m_bufa[i + m_offmax + 1] = m_bufa[i];
                }
                m_offa = m_offmax;
            }
            m_bufa[m_offa] = outval;
        }
        
        out[s] = outval;
    }
}

