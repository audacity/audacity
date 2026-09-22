/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file copyright 2008 QMUL.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_KLDIVERGENCE_H
#define QM_DSP_KLDIVERGENCE_H

#include <vector>

/**
 * Helper methods for calculating Kullback-Leibler divergences.
 */
class KLDivergence
{
public:
    KLDivergence() { }
    ~KLDivergence() { }

    /**
     * Calculate a symmetrised Kullback-Leibler divergence of Gaussian
     * models based on mean and variance vectors.  All input vectors
     * must be of equal size.
     */
    double distanceGaussian(const std::vector<double> &means1,
                            const std::vector<double> &variances1,
                            const std::vector<double> &means2,
                            const std::vector<double> &variances2);

    /**
     * Calculate a Kullback-Leibler divergence of two probability
     * distributions.  Input vectors must be of equal size.  If
     * symmetrised is true, the result will be the symmetrised
     * distance (equal to KL(d1, d2) + KL(d2, d1)).
     */
    double distanceDistribution(const std::vector<double> &d1,
                                const std::vector<double> &d2,
                                bool symmetrised);
};

#endif

