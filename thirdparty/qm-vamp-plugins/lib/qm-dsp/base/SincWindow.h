/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP library
    Centre for Digital Music, Queen Mary, University of London.
 
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_SINC_WINDOW_H
#define QM_DSP_SINC_WINDOW_H

#include <vector>

/**
 * A window containing values of the sinc function, i.e. sin(x)/x with
 * sinc(0) == 1, with x == 0 at the centre.
 */
class SincWindow
{
public:
    /**
     * Construct a windower of the given length, containing the values
     * of sinc(x) with x=0 in the middle, i.e. at sample (length-1)/2
     * for odd or (length/2)+1 for even length, such that the distance
     * from -pi to pi (the nearest zero crossings either side of the
     * peak) is p samples.
     */
    SincWindow(int length, double p) : m_length(length), m_p(p) { init(); }

    int getLength() const {
        return m_length;
    }

    const double *getWindow() const { 
        return m_window.data();
    }

    void cut(double *src) const { 
        cut(src, src); 
    }

    void cut(const double *src, double *dst) const {
        for (int i = 0; i < m_length; ++i) {
            dst[i] = src[i] * m_window[i];
        }
    }

private:
    int m_length;
    double m_p;
    std::vector<double> m_window;

    void init();
};

#endif
