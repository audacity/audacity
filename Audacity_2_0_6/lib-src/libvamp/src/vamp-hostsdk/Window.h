/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006-2011 Chris Cannam and QMUL.

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR
    ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
    CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

    Except as contained in this notice, the names of the Centre for
    Digital Music; Queen Mary, University of London; and Chris Cannam
    shall not be used in advertising or otherwise to promote the sale,
    use or other dealings in this Software without prior written
    authorization.
*/

#ifndef _WINDOW_H_
#define _WINDOW_H_

#include <vamp-hostsdk/hostguard.h>

#include <cmath>
#include <cstdlib>

_VAMP_SDK_HOSTSPACE_BEGIN(Window.h)

template <typename T>
class Window
{
public:
    enum WindowType {
        RectangularWindow,
        BartlettWindow,
        HammingWindow,
        HanningWindow,
        BlackmanWindow,
        NuttallWindow,
        BlackmanHarrisWindow
    };

    /**
     * Construct a windower of the given type.
     */
    Window(WindowType type, size_t size) : m_type(type), m_size(size) { encache(); }
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
    void cut(T *src, T *dst) const {
	for (size_t i = 0; i < m_size; ++i) dst[i] = src[i] * m_cache[i];
    }
    template <typename T0>
    void cut(T0 *src, T *dst) const {
	for (size_t i = 0; i < m_size; ++i) dst[i] = src[i] * m_cache[i];
    }

    T getArea() { return m_area; }
    T getValue(size_t i) { return m_cache[i]; }

    WindowType getType() const { return m_type; }
    size_t getSize() const { return m_size; }

protected:
    WindowType m_type;
    size_t m_size;
    T *m_cache;
    T m_area;
    
    void encache();
    void cosinewin(T *, T, T, T, T);
};

template <typename T>
void Window<T>::encache()
{
    int n = int(m_size);
    T *mult = new T[n];
    int i;
    for (i = 0; i < n; ++i) mult[i] = 1.0;

    switch (m_type) {
		
    case RectangularWindow:
	for (i = 0; i < n; ++i) {
	    mult[i] *= 0.5;
	}
	break;
	    
    case BartlettWindow:
	for (i = 0; i < n/2; ++i) {
	    mult[i] *= (i / T(n/2));
	    mult[i + n/2] *= (1.0 - (i / T(n/2)));
	}
	break;
	    
    case HammingWindow:
        cosinewin(mult, 0.54, 0.46, 0.0, 0.0);
	break;
	    
    case HanningWindow:
        cosinewin(mult, 0.50, 0.50, 0.0, 0.0);
	break;
	    
    case BlackmanWindow:
        cosinewin(mult, 0.42, 0.50, 0.08, 0.0);
	break;

    case NuttallWindow:
        cosinewin(mult, 0.3635819, 0.4891775, 0.1365995, 0.0106411);
	break;

    case BlackmanHarrisWindow:
        cosinewin(mult, 0.35875, 0.48829, 0.14128, 0.01168);
        break;
    }
	
    m_cache = mult;

    m_area = 0;
    for (int i = 0; i < n; ++i) {
        m_area += m_cache[i];
    }
    m_area /= n;
}

template <typename T>
void Window<T>::cosinewin(T *mult, T a0, T a1, T a2, T a3)
{
    int n = int(m_size);
    for (int i = 0; i < n; ++i) {
        mult[i] *= (a0
                    - a1 * cos((2 * M_PI * i) / n)
                    + a2 * cos((4 * M_PI * i) / n)
                    - a3 * cos((6 * M_PI * i) / n));
    }
}

_VAMP_SDK_HOSTSPACE_END(Window.h)

#endif
