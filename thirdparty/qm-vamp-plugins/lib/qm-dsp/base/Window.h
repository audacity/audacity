/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP library
    Centre for Digital Music, Queen Mary, University of London.
    This file Copyright 2006 Chris Cannam.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_WINDOW_H
#define QM_DSP_WINDOW_H

#include <cmath>
#include <iostream>
#include <map>
#include <vector>

enum WindowType {
    RectangularWindow,
    BartlettWindow,
    HammingWindow,
    HanningWindow,
    BlackmanWindow,
    BlackmanHarrisWindow,

    FirstWindow = RectangularWindow,
    LastWindow = BlackmanHarrisWindow
};

/**
 * Various shaped windows for sample frame conditioning, including
 * cosine windows (Hann etc) and triangular and rectangular windows.
 */
template <typename T>
class Window
{
public:
    /**
     * Construct a windower of the given type and size. 
     *
     * Note that the cosine windows are periodic by design, rather
     * than symmetrical. (A window of size N is equivalent to a
     * symmetrical window of size N+1 with the final element missing.)
     */
    Window(WindowType type, int size) : m_type(type), m_size(size) { encache(); }
    Window(const Window &w) : m_type(w.m_type), m_size(w.m_size) { encache(); }
    Window &operator=(const Window &w) {
        if (&w == this) return *this;
        m_type = w.m_type;
        m_size = w.m_size;
        encache();
        return *this;
    }
    virtual ~Window() { delete[] m_cache; }
    
    void cut(T *src) const { cut(src, src); }
    void cut(const T *src, T *dst) const {
        for (int i = 0; i < m_size; ++i) {
            dst[i] = src[i] * m_cache[i];
        }
    }

    WindowType getType() const { return m_type; }
    int getSize() const { return m_size; }

    std::vector<T> getWindowData() const {
        std::vector<T> d;
        for (int i = 0; i < m_size; ++i) {
            d.push_back(m_cache[i]);
        }
        return d;
    }

protected:
    WindowType m_type;
    int m_size;
    T *m_cache;
    
    void encache();
};

template <typename T>
void Window<T>::encache()
{
    int n = m_size;
    T *mult = new T[n];
    int i;
    for (i = 0; i < n; ++i) mult[i] = 1.0;

    switch (m_type) {
                
    case RectangularWindow:
        for (i = 0; i < n; ++i) {
            mult[i] = mult[i] * 0.5;
        }
        break;
            
    case BartlettWindow:
        if (n == 2) {
            mult[0] = mult[1] = 0; // "matlab compatible"
        } else if (n == 3) {
            mult[0] = 0;
            mult[1] = mult[2] = 2./3.;
        } else if (n > 3) {
            for (i = 0; i < n/2; ++i) {
                mult[i] = mult[i] * (i / T(n/2));
                mult[i + n - n/2] = mult[i + n - n/2] * (1.0 - (i / T(n/2)));
            }
        }
        break;
            
    case HammingWindow:
        if (n > 1) {
            for (i = 0; i < n; ++i) {
                mult[i] = mult[i] * (0.54 - 0.46 * cos(2 * M_PI * i / n));
            }
        }
        break;
            
    case HanningWindow:
        if (n > 1) {
            for (i = 0; i < n; ++i) {
                mult[i] = mult[i] * (0.50 - 0.50 * cos(2 * M_PI * i / n));
            }
        }
        break;
            
    case BlackmanWindow:
        if (n > 1) {
            for (i = 0; i < n; ++i) {
                mult[i] = mult[i] * (0.42 - 0.50 * cos(2 * M_PI * i / n)
                                     + 0.08 * cos(4 * M_PI * i / n));
            }
        }
        break;
            
    case BlackmanHarrisWindow:
        if (n > 1) {
            for (i = 0; i < n; ++i) {
                mult[i] = mult[i] * (0.35875
                                     - 0.48829 * cos(2 * M_PI * i / n)
                                     + 0.14128 * cos(4 * M_PI * i / n)
                                     - 0.01168 * cos(6 * M_PI * i / n));
            }
        }
        break;
    }
           
    m_cache = mult;
}

#endif
