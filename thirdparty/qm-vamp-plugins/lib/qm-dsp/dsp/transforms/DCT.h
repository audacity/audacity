/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file by Chris Cannam.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_DCT_H
#define QM_DSP_DCT_H

#include "FFT.h"

#include <vector>

class DCT
{
public:
    /**
     * Construct a DCT object to calculate the Discrete Cosine
     * Transform given input of size n samples. The transform is
     * implemented using an FFT of size 4n, for simplicity.
     */
    DCT(int n);

    ~DCT();

    /**
     * Carry out a type-II DCT of size n, where n is the value
     * provided to the constructor above.
     *
     * The in and out pointers must point to (enough space for) n
     * values.
     */
    void forward(const double *in, double *out);

    /**
     * Carry out a type-II unitary DCT of size n, where n is the value
     * provided to the constructor above. This is a scaled version of
     * the type-II DCT corresponding to a transform with an orthogonal
     * matrix. This is the transform implemented by the dct() function
     * in MATLAB.
     *
     * The in and out pointers must point to (enough space for) n
     * values.
     */
    void forwardUnitary(const double *in, double *out);

    /**
     * Carry out a type-III (inverse) DCT of size n, where n is the
     * value provided to the constructor above.
     *
     * The in and out pointers must point to (enough space for) n
     * values.
     */
    void inverse(const double *in, double *out);

    /**
     * Carry out a type-III (inverse) unitary DCT of size n, where n
     * is the value provided to the constructor above. This is the
     * inverse of forwardUnitary().
     *
     * The in and out pointers must point to (enough space for) n
     * values.
     */
    void inverseUnitary(const double *in, double *out);

private:
    int m_n;
    double m_scale;
    std::vector<double> m_scaled;
    std::vector<double> m_time2;
    std::vector<double> m_freq2r;
    std::vector<double> m_freq2i;
    FFTReal m_fft;
};

#endif
