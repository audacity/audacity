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

#ifndef QM_DSP_TONALESTIMATOR_H
#define QM_DSP_TONALESTIMATOR_H

#include <valarray>
#include <numeric>
#include <algorithm>
#include <iostream>

class ChromaVector : public std::valarray<double>
{
public:
    ChromaVector(size_t uSize = 12) : std::valarray<double>() {
        resize(uSize, 0.0f);
    }
        
    virtual ~ChromaVector() {};
        
    void printDebug() {
        for (int i = 0; i < int(size()); i++) {
            std::cout <<  (*this)[i] << ";";
        }
        std::cout << std::endl;
    }
        
    void normalizeL1() {
        // normalize the chroma vector (L1 norm)
        double dSum = 0.0;
        
        for (size_t i = 0; i < 12; (dSum += std::abs((*this)[i++]))) ;
        for (size_t i = 0; i < 12; dSum > 0.0000001?((*this)[i] /= dSum):(*this)[i]=0.0, i++) ;
    }

    void clear() {
        for (size_t i = 0; i < 12; ++i) (*this)[i] = 0.0;
    }
};

class TCSVector : public std::valarray<double>
{
public:
    TCSVector() : std::valarray<double>() {
        resize(6, 0.0f);
    }
        
    virtual ~TCSVector() {};

    void printDebug() {
        for (int i = 0; i < int(size()); i++) {
            std::cout <<  (*this)[i] << ";";
        }
        std::cout << std::endl;
    }
        
    double magnitude() const {
        double dMag = 0.0;
                
        for (size_t i = 0; i < 6; i++) {
            dMag += std::pow((*this)[i], 2.0);
        }
                
        return std::sqrt(dMag);
    }
};

class TonalEstimator
{
public:
    TonalEstimator();
    virtual ~TonalEstimator();
    TCSVector transform2TCS(const ChromaVector& rVector);
    
protected:
    std::valarray< std::valarray<double> > m_Basis;
};

#endif // _TONALESTIMATOR_
