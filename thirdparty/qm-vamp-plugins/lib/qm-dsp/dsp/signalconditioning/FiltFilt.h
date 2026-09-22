/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file 2005-2006 Christian Landone.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_FILTFILT_H
#define QM_DSP_FILTFILT_H

#include "Filter.h"

/**
 * Zero-phase digital filter, implemented by processing the data
 * through a filter specified by the given filter parameters (see
 * Filter) and then processing it again in reverse.
 */
class FiltFilt  
{
public:
    FiltFilt(Filter::Parameters);
    virtual ~FiltFilt();

    void process(const double *const QM_R__ src,
                 double *const QM_R__ dst,
                 const int length);

private:
    Filter m_filter;
    int m_ord;
};

#endif
