/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
 * ClusterMeltSegmenter.h
 *
 * Created by Mark Levy on 23/03/2006.
 * Copyright 2006 Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
 */

#ifndef QM_DSP_CLUSTER_MELT_SEGMENTER_H
#define QM_DSP_CLUSTER_MELT_SEGMENTER_H

#include <vector>

#include "segment.h"
#include "Segmenter.h"
#include "hmm/hmm.h"
#include "base/Window.h"

class Decimator;
class ConstantQ;
class MFCC;
class FFTReal;

class ClusterMeltSegmenterParams
// defaults are sensible for 11025Hz with 0.2 second hopsize
{
public:
    ClusterMeltSegmenterParams() : 
        featureType(FEATURE_TYPE_CONSTQ),
        hopSize(0.2),
        windowSize(0.6),
        fmin(62),
        fmax(16000), 
        nbins(8),
        ncomponents(20),
        nHMMStates(40),
        nclusters(10),
        histogramLength(15),
        neighbourhoodLimit(20) { }
    feature_types featureType;
    double hopSize;     // in secs
    double windowSize;  // in secs
    int fmin;
    int fmax;
    int nbins;
    int ncomponents;
    int nHMMStates;
    int nclusters;
    int histogramLength;
    int neighbourhoodLimit;
};

class ClusterMeltSegmenter : public Segmenter
{
public:
    ClusterMeltSegmenter(ClusterMeltSegmenterParams params);
    virtual ~ClusterMeltSegmenter();
    virtual void initialise(int samplerate);
    virtual int getWindowsize();
    virtual int getHopsize();
    virtual void extractFeatures(const double* samples, int nsamples);
    void setFeatures(const std::vector<std::vector<double> >& f);         // provide the features yourself
    virtual void segment();             // segment into default number of segment-types
    void segment(int m);                // segment into m segment-types
    int getNSegmentTypes() { return nclusters; }

protected:
    void makeSegmentation(int* q, int len);
        
    void extractFeaturesConstQ(const double *, int);
    void extractFeaturesMFCC(const double *, int);

    Window<double> *window;
    FFTReal *fft;
    ConstantQ* constq; 
    MFCC* mfcc;
    model_t* model;                             // the HMM
    int* q;                                     // the decoded HMM state sequence
    std::vector<std::vector<double> > histograms; 
    
    feature_types featureType;  
    double hopSize;             // in seconds
    double windowSize;  // in seconds
    
    // constant-Q parameters
    int fmin;
    int fmax;
    int nbins;
    int ncoeff;
    
    // PCA parameters
    int ncomponents;
    
    // HMM parameters
    int nHMMStates;
    
    // clustering parameters
    int nclusters;
    int histogramLength;
    int neighbourhoodLimit;

    Decimator *decimator;
};

#endif
