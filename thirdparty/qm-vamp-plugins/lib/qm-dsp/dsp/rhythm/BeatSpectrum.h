/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file copyright 2008 Kurt Jacobson and QMUL.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_BEATSPECTRUM_H
#define QM_DSP_BEATSPECTRUM_H

#include <vector>

/**
 * Given a matrix of "feature values", calculate a self-similarity
 * vector.  The resulting vector will have half as many elements as
 * the number of columns in the matrix.  This is based on the
 * SoundBite rhythmic similarity code.
 */

class BeatSpectrum
{
public:
    BeatSpectrum() { }
    ~BeatSpectrum() { }

    std::vector<double> process(const std::vector<std::vector<double> > &inmatrix);

};

#endif


