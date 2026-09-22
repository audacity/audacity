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

#include "SincWindow.h"

#include <cmath>

void
SincWindow::init()
{
    if (m_length < 1) {
        return;
    } else if (m_length < 2) {
        m_window.push_back(1);
        return;
    } else {

        int n0 = (m_length % 2 == 0 ? m_length/2 : (m_length - 1)/2);
        int n1 = (m_length % 2 == 0 ? m_length/2 : (m_length + 1)/2);
        double m = 2 * M_PI / m_p;

        for (int i = 0; i < n0; ++i) {
            double x = ((m_length / 2) - i) * m;
            m_window.push_back(sin(x) / x);
        }

        m_window.push_back(1.0);

        for (int i = 1; i < n1; ++i) {
            double x = i * m;
            m_window.push_back(sin(x) / x);
        }
    }
}

