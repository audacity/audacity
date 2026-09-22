/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file copyright 2006 Martin Gasser.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_CHANGEDETECTIONFUNCTION_H
#define QM_DSP_CHANGEDETECTIONFUNCTION_H

#include "TCSgram.h"

#include <valarray>

typedef std::valarray<double> ChangeDistance;

struct ChangeDFConfig
{
    int smoothingWidth;
};

class ChangeDetectionFunction
{
public:
    ChangeDetectionFunction(ChangeDFConfig);
    ~ChangeDetectionFunction();
    ChangeDistance process(const TCSGram& rTCSGram);
private:
    void setFilterWidth(const int iWidth);
        
private:
    std::valarray<double> m_vaGaussian;
    double m_dFilterSigma;
    int m_iFilterWidth;
};

#endif // _CHANGDETECTIONFUNCTION_
