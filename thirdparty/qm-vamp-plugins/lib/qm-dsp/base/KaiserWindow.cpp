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

#include "KaiserWindow.h"

#include "maths/MathUtilities.h"

KaiserWindow::Parameters
KaiserWindow::parametersForTransitionWidth(double attenuation,
                                           double transition)
{
    Parameters p;
    p.length = 1 + (attenuation > 21.0 ?
                    ceil((attenuation - 7.95) / (2.285 * transition)) :
                    ceil(5.79 / transition));
    p.beta = (attenuation > 50.0 ? 
              0.1102 * (attenuation - 8.7) :
              attenuation > 21.0 ? 
              0.5842 * pow(attenuation - 21.0, 0.4) + 0.07886 * (attenuation - 21.0) :
              0);
    return p;
}

static double besselTerm(double x, int i)
{
    if (i == 0) {
        return 1;
    } else {
        double f = MathUtilities::factorial(i);
        return pow(x/2, i*2) / (f*f);
    }
}

static double bessel0(double x)
{
    double b = 0.0;
    for (int i = 0; i < 20; ++i) {
        b += besselTerm(x, i);
    }
    return b;
}

void
KaiserWindow::init()
{
    double denominator = bessel0(m_beta);
    bool even = (m_length % 2 == 0);
    for (int i = 0; i < (even ? m_length/2 : (m_length+1)/2); ++i) {
        double k = double(2*i) / double(m_length-1) - 1.0;
        m_window.push_back(bessel0(m_beta * sqrt(1.0 - k*k)) / denominator);
    }
    for (int i = 0; i < (even ? m_length/2 : (m_length-1)/2); ++i) {
        m_window.push_back(m_window[int(m_length/2) - i - 1]);
    }
}
