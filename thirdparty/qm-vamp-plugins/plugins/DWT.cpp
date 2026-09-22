/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
  QM Vamp Plugin Set

  Centre for Digital Music, Queen Mary, University of London.
  This file copyright 2009 Thomas Wilmering.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#include "DWT.h"

#include <cmath>

using std::string;
using std::vector;
using std::cerr;
using std::endl;

DWT::DWT(float inputSampleRate) :
    Plugin(inputSampleRate),
    m_stepSize(0),
    m_blockSize(0)
{
    m_scales = 10;
    m_flength = 0;
    m_wavelet = Wavelet::Haar;
    m_threshold = 0;
    m_absolute = 0;
}

DWT::~DWT()
{
}

string
DWT::getIdentifier() const
{
    return "qm-dwt";
}

string
DWT::getName() const
{
    return "Discrete Wavelet Transform";
}

string
DWT::getDescription() const
{
    return "Visualisation by scalogram";
}

string
DWT::getMaker() const
{
    return "Queen Mary, University of London";
}

int
DWT::getPluginVersion() const
{
    return 1;
}

string
DWT::getCopyright() const
{
    return "Plugin by Thomas Wilmering.  Copyright (c) 2009 Thomas Wilmering and QMUL - All Rights Reserved";
}

size_t 
DWT::getPreferredBlockSize() const 
{ 
    size_t s = (1 << m_scales);
    while (s < 1024) s *= 2;
    return s;
} 

size_t 
DWT::getPreferredStepSize() const 
{ 
    return 0;  
} 

bool
DWT::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (channels < getMinChannelCount() ||
        channels > getMaxChannelCount()) return false;

    if (m_inputSampleRate > 1000000) { // somewhat arbitrarily
        std::cerr << "DWT::initialise: ERROR: Maximum sample rate exceeded" << std::endl;
        return false;
    }
	
    if ((1U << m_scales) > blockSize) {
        std::cerr << "DWT::initialise: ERROR: Block size must be at least 2^scales (specified block size " << blockSize << " < " << (1 << m_scales) << ")" << std::endl;
        return false;
    }

    m_stepSize = stepSize;
    m_blockSize = blockSize;

    Wavelet::createDecompositionFilters(m_wavelet, m_lpd, m_hpd);

    m_flength = m_lpd.size(); // or m_hpd.size()
	
    m_samplePass.resize(m_scales);				// resize buffer for samples to pass to next block
	
    for (int i=0; i<m_scales; ++i) {
        m_samplePass[i].resize(m_flength-2, 0.0);
    }

    return true;	
}

void
DWT::reset()
{
    m_samplePass.clear();

    m_samplePass.resize(m_scales);
	
    for (int i=0; i<m_scales; ++i) {
        m_samplePass[i].resize(m_flength-2, 0.0);
    }
}

DWT::OutputList
DWT::getOutputDescriptors() const
{
    OutputList list;
	
    OutputDescriptor sg;
    sg.identifier = "wcoeff";
    sg.name = "Wavelet Coefficients";
    sg.description = "Wavelet coefficients";
    sg.unit = "";
    sg.hasFixedBinCount = true;              // depends on block size
    sg.binCount = m_scales; 	// number of scales
    sg.hasKnownExtents = false;
    sg.isQuantized = false;
    sg.sampleType = OutputDescriptor::FixedSampleRate; 
    sg.sampleRate = .5 * m_inputSampleRate;
	
    list.push_back(sg);
	
    return list;
}


DWT::ParameterList
DWT::getParameterDescriptors() const
{
    ParameterList list;
 
    ParameterDescriptor d;
    d.identifier = "scales";
    d.name = "Scales";
    d.description = "Scale depth";
    d.unit = "";
    d.minValue = 1.0f;
    d.maxValue = 16.0f;
    d.defaultValue = 10.0f;
    d.isQuantized = true;
    d.quantizeStep = 1.0f;
    list.push_back(d);
 
    d.identifier = "wavelet";
    d.name = "Wavelet";
    d.description = "Wavelet type to use";
    d.unit = "";
    d.minValue = 0.f;
    d.maxValue = int(Wavelet::LastType);
    d.defaultValue = int(Wavelet::Haar);
    d.isQuantized = true;
    d.quantizeStep = 1.0f;

    for (int i = 0; i <= int(Wavelet::LastType); ++i) {
        d.valueNames.push_back(Wavelet::getWaveletName(Wavelet::Type(i)));
    }
    list.push_back(d);
    d.valueNames.clear();

    d.identifier = "threshold";
    d.name = "Threshold";
    d.description = "Wavelet coefficient threshold";
    d.unit = "";
    d.minValue = 0.0f;
    d.maxValue = 0.01f;
    d.defaultValue = 0.0f;
    d.isQuantized = false;
    list.push_back(d);

    d.identifier = "absolute";
    d.name = "Absolute values";
    d.description = "Return absolute values";
    d.unit = "";
    d.minValue = 0.0f;
    d.maxValue = 1.00f;
    d.defaultValue = 0.0f;
    d.isQuantized = true;
    d.quantizeStep = 1.0f;
    list.push_back(d);
 
    return list;
}
 
void DWT::setParameter(std::string paramid, float newval)
{
    if (paramid == "scales") {
        m_scales = newval;
    }
    else if (paramid == "wavelet") {
        m_wavelet = (Wavelet::Type)(int(newval + 0.1));
    }
    else if (paramid == "threshold") {
        m_threshold = newval;
    }
    else if (paramid == "absolute") {
        m_absolute = newval;
    }
}
 
float DWT::getParameter(std::string paramid) const
{
    if (paramid == "scales") {
        return m_scales;
    } 
    else if (paramid == "wavelet") {
        return int(m_wavelet);
    }
    else if (paramid == "threshold") {
        return m_threshold;
    } 
    else if (paramid == "absolute") {
        return m_absolute;
    } 
 
    return 0.0f; 
}
 
 
DWT::FeatureSet
DWT::process(const float *const *inputBuffers,
             Vamp::RealTime)
{
    FeatureSet fs;
	
    if (m_blockSize == 0) {
        cerr << "ERROR: DWT::process: Not initialised" << endl;
        return fs;
    } 
	
    int s = m_scales;
    int b = m_blockSize;
    int b_init = b;

    if ((1 << s) > b) b = 1 << s;												// correct blocksize if smaller than 2^(max scale)

//--------------------------------------------------------------------------------------------------	

    float tempDet;
    float aTempDet;
    int outloc;
    int halfblocksize = int(.5 * b);
    int fbufloc;
    int fbufloc2;    
		
    vector< vector<float> > wCoefficients(m_scales);						// result
    vector<float> tempAprx(halfblocksize,0.0);								// approximation
    vector<float> fbuf(b+m_flength-2,0.0);									// input buffer

    for (int n=m_flength-2; n<b+m_flength-2; n++)								// copy input buffer to dwt input
        fbuf[n] = inputBuffers[0][n-m_flength+2];		
	
    for (int scale=0; scale<m_scales; ++scale)								// do for each scale
    {	
        for (int n=0; n<m_flength-2; ++n)										// get samples from previous block
            fbuf[n]  = m_samplePass[scale][n];
		
		
        if ((m_flength-2)<b)													// pass samples to next block
            for (int n=0; n<m_flength-2; ++n)					
                m_samplePass[scale][n] = fbuf[b+n];
        else {
            for (int n=0; n<b; ++n)										// if number of samples to pass > blocksize
                m_samplePass[scale].push_back(fbuf[m_flength-2+n]);
            m_samplePass[scale].erase (m_samplePass[scale].begin(),m_samplePass[scale].begin()+b);
        }
					
        for (int n=0; n<halfblocksize; ++n)	{								// do for every other sample of the input buffer
            tempDet = 0;
            fbufloc = 2*n+m_flength-1;
            for (int m=0; m<m_flength; ++m)	{								// Convolve the sample with filter coefficients
                fbufloc2 = fbufloc - m;
                tempAprx[n] += fbuf[fbufloc2] * m_lpd[m];						// approximation
                tempDet += fbuf[fbufloc2] * m_hpd[m];							// detail
            }
		
            aTempDet = fabs(tempDet);
            if (m_absolute == 1) tempDet = aTempDet;
			
			
            if (aTempDet < m_threshold) tempDet = 0;							// simple hard thresholding, same for each scale
            wCoefficients[scale].push_back(tempDet);
        }
																				
        if (scale+1<m_scales) {												// prepare variables for next scale
            b = b >> 1;														// the approximation in tmpfwd is stored as 
            halfblocksize = halfblocksize >> 1;								// input for next level
	
            for (int n=m_flength-2; n<b+m_flength-2; n++)						// copy approximation to dwt input
                fbuf[n] = tempAprx[n-m_flength+2];
			
            //vector<float>(b+m_flength-2).swap(fbuf);
            vector<float>(halfblocksize).swap(tempAprx);					// set new size with zeros
        }
    }
	
	
//-----------------------------------------------------------------------------------------
	
    halfblocksize = int(.5 * b_init);
	
    for (int m = 0; m<halfblocksize; m++) {
		
        Feature feature;
        feature.hasTimestamp = false;
		
        for (int j = 0; j < s; j++) {
            outloc = m / (1 << j);									// This one pushes a single result bin 
            // onto the top of a feature column
            feature.values.push_back(wCoefficients[j][outloc]);				// each coefficient on higher scales need 
        }																	// to be copied multiple times to feature columns
        fs[0].push_back(feature);
    }
    return fs;
}



DWT::FeatureSet
DWT::getRemainingFeatures()
{
    int s = m_scales;

    FeatureSet fs;
	
/*	
	int b = 1;							
	while (b<((m_flength-1) * (1 << s))) {				//set blocksize to tail length
        b= (b << 1);
	}
	int b_init = b;
	
*/
    int b = m_blockSize;
    int b_init = b;
    int tailIterations = int(((m_flength-1) * (1 << s)) / b) + 1;   // number of iterations for tail 

	
    for(int m=0; m<tailIterations; ++m)
    {

	b = b_init;
	
	//-------------------------------------------------------------------------------------------	
	float tempDet;
        float aTempDet;
	int outloc;
	int halfblocksize = int(.5 * b);
	int fbufloc;
	int fbufloc2;    
        int len = m_flength;
	
	vector< vector<float> > wCoefficients(m_scales);						// result
	vector<float> tempAprx(halfblocksize,0.0);								// approximation
	vector<float> fbuf(b+len-2,0.0);									// input buffer
	
	//for (int n=len-2; n<b+len-2; n++)								// copy input buffer to dwt input
	//	fbuf[n] = 0; //inputBuffers[0][n-len+2];		
	
	for (int scale=0; scale<m_scales; ++scale)								// do for each scale
	{	
            for (int n=0; n<len-2; ++n)										// get samples from previous block
                fbuf[n]  = m_samplePass[scale][n];
		
		
            if ((len-2)<b)													// pass samples to next block
                for (int n=0; n<len-2; ++n)					
                    m_samplePass[scale][n] = fbuf[b+n];
            else {
                for (int n=0; n<b; ++n)										// if number of samples to pass > blocksize
                    m_samplePass[scale].push_back(fbuf[len-2+n]);
                m_samplePass[scale].erase (m_samplePass[scale].begin(),m_samplePass[scale].begin()+b);
            }
		
            for (int n=0; n<halfblocksize; ++n)	{								// do for every other sample of the input buffer
                tempDet = 0;
                fbufloc = 2*n+len-1;
                for (int m=0; m<len; ++m)	{								// Convolve the sample with filter coefficients
                    fbufloc2 = fbufloc - m;
                    tempAprx[n] += fbuf[fbufloc2] * m_lpd[m];						// approximation
                    tempDet += fbuf[fbufloc2] * m_hpd[m];							// detail
                }

                aTempDet = fabs(tempDet);
                if (m_absolute == 1) tempDet = aTempDet;
                if (aTempDet < m_threshold) tempDet = 0;							// simple hard thresholding, same for each scale
                wCoefficients[scale].push_back(tempDet);
            }
		
	    if (scale+1<m_scales) {												// prepare variables for next scale
                b = b >> 1;														// the approximation in tmpfwd is stored as 
                halfblocksize = halfblocksize >> 1;								// input for next level
			
                for (int n=len-2; n<b+len-2; n++)						// copy approximation to dwt input
                    fbuf[n] = tempAprx[n-len+2];
			
                //vector<float>(b+len-2).swap(fbuf);
                vector<float>(halfblocksize).swap(tempAprx);					// set new size with zeros
            }
	
	}
	
//-----------------------------------------------------------------------------------------
	
	halfblocksize = int(.5 * b_init + 0.1);
	
	for (int m = 0; m<halfblocksize; m++) {
		
            Feature feature;
            feature.hasTimestamp = false;
		
            for (int j = 0; j < s; j++) {
                outloc = m / (1 << j);									// This one pushes a single result bin 
                // onto the top of a feature column
                feature.values.push_back(wCoefficients[j][outloc]);				// each coefficient on higher scales need 
            }																	// to be copied multiple times to feature columns
            fs[0].push_back(feature);
        }
    }
    return fs;

}

