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

#include "ChangeDetectionFunction.h"

ChangeDetectionFunction::ChangeDetectionFunction(ChangeDFConfig config) :
    m_dFilterSigma(0.0), m_iFilterWidth(0)
{
    setFilterWidth(config.smoothingWidth);
}

ChangeDetectionFunction::~ChangeDetectionFunction()
{
}

void ChangeDetectionFunction::setFilterWidth(const int iWidth)
{
    m_iFilterWidth = iWidth*2+1;
        
    // it is assumed that the gaussian is 0 outside of +/- FWHM
    // => filter width = 2*FWHM = 2*2.3548*sigma
    m_dFilterSigma = double(m_iFilterWidth) / double(2*2.3548);
    m_vaGaussian.resize(m_iFilterWidth);
        
    double dScale = 1.0 / (m_dFilterSigma*sqrt(2*M_PI));
        
    for (int x = -(m_iFilterWidth-1)/2; x <= (m_iFilterWidth-1)/2; x++) {
        double w = dScale * std::exp ( -(x*x)/(2*m_dFilterSigma*m_dFilterSigma) );
        m_vaGaussian[x + (m_iFilterWidth-1)/2] = w;
    }
        
#ifdef DEBUG_CHANGE_DETECTION_FUNCTION
    std::cerr << "Filter sigma: " << m_dFilterSigma << std::endl;
    std::cerr << "Filter width: " << m_iFilterWidth << std::endl;
#endif
}


ChangeDistance ChangeDetectionFunction::process(const TCSGram& rTCSGram)
{
    ChangeDistance retVal;
    retVal.resize(rTCSGram.getSize(), 0.0);
        
    TCSGram smoothedTCSGram;

    for (int iPosition = 0; iPosition < rTCSGram.getSize(); iPosition++) {
        
        int iSkipLower = 0;
        
        int iLowerPos = iPosition - (m_iFilterWidth-1)/2;
        int iUpperPos = iPosition + (m_iFilterWidth-1)/2;
        
        if (iLowerPos < 0) {
            iSkipLower = -iLowerPos;
            iLowerPos = 0;
        }
        
        if (iUpperPos >= rTCSGram.getSize()) {
            int iMaxIndex = rTCSGram.getSize() - 1;
            iUpperPos = iMaxIndex;
        }
        
        TCSVector smoothedVector;

        // for every bin of the vector, calculate the smoothed value
        for (int iPC = 0; iPC < 6; iPC++) {       

            size_t j = 0;
            double dSmoothedValue = 0.0;
            TCSVector rCV;
                
            for (int i = iLowerPos; i <= iUpperPos; i++) {
                rTCSGram.getTCSVector(i, rCV);
                dSmoothedValue += m_vaGaussian[iSkipLower + j++] * rCV[iPC];
            }

            smoothedVector[iPC] = dSmoothedValue;
        }
                
        smoothedTCSGram.addTCSVector(smoothedVector);
    }

    for (int iPosition = 0; iPosition < rTCSGram.getSize(); iPosition++) {
        
        /*
          TODO: calculate a confidence measure for the current estimation
          if the current estimate is not confident enough, look further into the future/the past
          e.g., High frequency content, zero crossing rate, spectral flatness
        */
                
        TCSVector nextTCS;
        TCSVector previousTCS;
                
        int iWindow = 1;

        // while (previousTCS.magnitude() < 0.1 && (iPosition-iWindow) > 0)
        {
            smoothedTCSGram.getTCSVector(iPosition-iWindow, previousTCS);
            // std::cout << previousTCS.magnitude() << std::endl;
            iWindow++;
        }
                
        iWindow = 1;
                
        // while (nextTCS.magnitude() < 0.1 && (iPosition+iWindow) < (rTCSGram.getSize()-1) )
        {
            smoothedTCSGram.getTCSVector(iPosition+iWindow, nextTCS);
            iWindow++;
        }

        double distance = 0.0;
        // Euclidean distance
        for (size_t j = 0; j < 6; j++) {
            distance += std::pow(nextTCS[j] - previousTCS[j], 2.0);
        }
        
        retVal[iPosition] = std::pow(distance, 0.5);
    }

    return retVal;
}
