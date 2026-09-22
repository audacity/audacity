/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file Copyright 2010 Chris Cannam.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_MEDIAN_FILTER_H
#define QM_DSP_MEDIAN_FILTER_H

#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <vector>

template <typename T>
class MedianFilter
{
public:
    MedianFilter(int size, float percentile = 50.f) :
        m_size(size),
        m_frame(new T[size]),
        m_sorted(new T[size]),
        m_sortend(m_sorted + size - 1) {
        setPercentile(percentile);
        reset();
    }

    ~MedianFilter() { 
        delete[] m_frame;
        delete[] m_sorted;
    }

    void setPercentile(float p) {
        m_index = int((m_size * p) / 100.f);
        if (m_index >= m_size) m_index = m_size-1;
        if (m_index < 0) m_index = 0;
    }

    void push(T value) {
        if (value != value) {
            std::cerr << "WARNING: MedianFilter::push: attempt to push NaN, pushing zero instead" << std::endl;
            // we do need to push something, to maintain the filter length
            value = T();
        }
        drop(m_frame[0]);
        const int sz1 = m_size-1;
        for (int i = 0; i < sz1; ++i) m_frame[i] = m_frame[i+1];
        m_frame[m_size-1] = value;
        put(value);
    }

    T get() const {
        return m_sorted[m_index];
    }

    int getSize() const {
        return m_size; 
    }

    T getAt(float percentile) {
        int ix = int((m_size * percentile) / 100.f);
        if (ix >= m_size) ix = m_size-1;
        if (ix < 0) ix = 0;
        return m_sorted[ix];
    }

    void reset() {
        for (int i = 0; i < m_size; ++i) m_frame[i] = 0;
        for (int i = 0; i < m_size; ++i) m_sorted[i] = 0;
    }

    static std::vector<T> filter(int size, const std::vector<T> &in) {
        std::vector<T> out;
        MedianFilter<T> f(size);
        for (int i = 0; i < int(in.size()); ++i) {
            f.push(in[i]);
            T median = f.get();
            if (i >= size/2) out.push_back(median);
        }
        while (out.size() < in.size()) {
            f.push(T());
            out.push_back(f.get());
        }
        return out;
    }

private:
    const int m_size;
    T *const m_frame;
    T *const m_sorted;
    T *const m_sortend;
    int m_index;

    void put(T value) {
        // precondition: m_sorted contains m_size-1 values, packed at start
        // postcondition: m_sorted contains m_size values, one of which is value
        T *point = std::lower_bound(m_sorted, m_sortend, value);
        const int n = m_sortend - point;
        for (int i = n; i > 0; --i) point[i] = point[i-1];
        *point = value;
    }

    void drop(T value) {
        // precondition: m_sorted contains m_size values, one of which is value
        // postcondition: m_sorted contains m_size-1 values, packed at start
        T *point = std::lower_bound(m_sorted, m_sortend + 1, value);
        if (*point != value) {
            std::cerr << "WARNING: MedianFilter::drop: *point is " << *point
                      << ", expected " << value << std::endl;
        }
        const int n = m_sortend - point;
        for (int i = 0; i < n; ++i) point[i] = point[i+1];
        *m_sortend = T(0);
    }

    MedianFilter(const MedianFilter &); // not provided
    MedianFilter &operator=(const MedianFilter &); // not provided
};

#endif

