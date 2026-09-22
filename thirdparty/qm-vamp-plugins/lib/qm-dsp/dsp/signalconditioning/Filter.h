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

#ifndef QM_DSP_FILTER_H
#define QM_DSP_FILTER_H

#include "base/Restrict.h"

#include <vector>

class Filter
{
public:
    struct Parameters {
        std::vector<double> a;
        std::vector<double> b;
    };

    /**
     * Construct an IIR filter with numerators b and denominators
     * a. The filter will have order b.size()-1. To make an FIR
     * filter, leave the vector a in the param struct empty.
     * Otherwise, a and b must have the same number of values.
     */
    Filter(Parameters params);
    
    ~Filter();

    void reset();

    /**
     * Filter the input sequence \arg in of length \arg n samples, and
     * write the resulting \arg n samples into \arg out. There must be
     * enough room in \arg out for \arg n samples to be written.
     */
    void process(const double *const QM_R__ in,
                 double *const QM_R__ out,
                 const int n);

    int getOrder() const { return m_order; }
    
private:
    int m_order;
    int m_sz;
    std::vector<double> m_a;
    std::vector<double> m_b;
    std::vector<double> m_bufa;
    std::vector<double> m_bufb;
    int m_offa;
    int m_offb;
    int m_offmax;
    bool m_fir;

    Filter(const Filter &); // not supplied
    Filter &operator=(const Filter &); // not supplied
};
    
#endif
