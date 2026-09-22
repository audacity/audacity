/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file copyright 2008 Kurt Jacobson.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_COSINEDISTANCE_H
#define QM_DSP_COSINEDISTANCE_H

#include <vector>
#include <math.h>

class CosineDistance
{
public:
    CosineDistance() { }
    ~CosineDistance() { }

    double distance(const std::vector<double> &v1,
                    const std::vector<double> &v2);

protected:
    double dist, dDenTot, dDen1, dDen2, dSum1;
};

#endif

