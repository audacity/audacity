/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file copyright 2009 Thomas Wilmering.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_WAVELET_H
#define QM_DSP_WAVELET_H

#include <string>
#include <vector>

class Wavelet
{
public:
    enum Type {
        Haar = 0,
        Daubechies_2,
        Daubechies_3,
        Daubechies_4,
        Daubechies_5,
        Daubechies_6,
        Daubechies_7,
        Daubechies_8,
        Daubechies_9,
        Daubechies_10,
        Daubechies_20,
        Daubechies_40,
        Symlet_2,
        Symlet_3,
        Symlet_4,
        Symlet_5,
        Symlet_6,
        Symlet_7,
        Symlet_8,
        Symlet_9,
        Symlet_10,
        Symlet_20,
        Symlet_30,
        Coiflet_1,
        Coiflet_2,
        Coiflet_3,
        Coiflet_4,
        Coiflet_5,
        Biorthogonal_1_3,
        Biorthogonal_1_5,
        Biorthogonal_2_2,
        Biorthogonal_2_4,
        Biorthogonal_2_6,
        Biorthogonal_2_8,
        Biorthogonal_3_1,
        Biorthogonal_3_3,
        Biorthogonal_3_5,
        Biorthogonal_3_7,
        Biorthogonal_3_9,
        Biorthogonal_4_4,
        Biorthogonal_5_5,
        Biorthogonal_6_8,
        Meyer,

        LastType = Meyer
    };

    static std::string getWaveletName(Type);

    static void createDecompositionFilters(Type,
                                           std::vector<double> &lpd,
                                           std::vector<double> &hpd);
};

#endif
