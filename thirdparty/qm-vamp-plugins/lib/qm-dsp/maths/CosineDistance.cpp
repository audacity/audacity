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

#include "CosineDistance.h"

#include <iostream>
#include <limits>

using std::cerr;
using std::vector;

double CosineDistance::distance(const vector<double> &v1,
                                const vector<double> &v2)
{
    dist = 1.0; dDenTot = 0; dDen1 = 0; dDen2 = 0; dSum1 =0;
    double small = 1e-20;

    //check if v1, v2 same size
    if (v1.size() != v2.size())
    {
        cerr << "CosineDistance::distance: ERROR: vectors not the same size\n";
        return 1.0;
    }
    else
    {
        for(int i=0; i<int(v1.size()); i++)
        {
            dSum1 += v1[i]*v2[i];
            dDen1 += v1[i]*v1[i];
            dDen2 += v2[i]*v2[i];
        }
        dDenTot = sqrt(fabs(dDen1*dDen2)) + small;
        dist = 1-((dSum1)/dDenTot);
        return dist;
    }
}
