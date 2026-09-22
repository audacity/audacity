/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
 * SimilarityPlugin.h
 *
 * Copyright 2008 Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
 */

#ifndef _SIMILARITY_PLUGIN_H_
#define _SIMILARITY_PLUGIN_H_

#include <vamp-sdk/Plugin.h>
#include <vamp-sdk/RealTime.h>

#include <vector>
#include <deque>

class MFCC;
class Chromagram;
class Decimator;

class SimilarityPlugin : public Vamp::Plugin
{
public:
    SimilarityPlugin(float inputSampleRate);
    virtual ~SimilarityPlugin();
	
    bool initialise(size_t channels, size_t stepSize, size_t blockSize);
    void reset();
	
    std::string getIdentifier() const;
    std::string getName() const;
    std::string getDescription() const;
    std::string getMaker() const;
    int getPluginVersion() const;
    std::string getCopyright() const;
	
    size_t getPreferredStepSize() const;
    size_t getPreferredBlockSize() const;
    InputDomain getInputDomain() const { return TimeDomain; }
    
    size_t getMinChannelCount() const;
    size_t getMaxChannelCount() const;

    SimilarityPlugin::ParameterList getParameterDescriptors() const;
    float getParameter(std::string param) const;
    void setParameter(std::string param, float value);
    
    OutputList getOutputDescriptors() const;
    
    FeatureSet process(const float *const *inputBuffers, Vamp::RealTime timestamp);
    
    FeatureSet getRemainingFeatures();
	
protected:
    int getDecimationFactor() const;
    
    enum Type {
        TypeMFCC,
        TypeChroma
    };

    void calculateBlockSize() const;
    bool needRhythm() const { return m_rhythmWeighting > m_noRhythm; }
    bool needTimbre() const { return m_rhythmWeighting < m_allRhythm; }

    Type m_type;
    MFCC *m_mfcc;
    MFCC *m_rhythmfcc;
    Chromagram *m_chromagram;
    Decimator *m_decimator;
    int m_featureColumnSize;
    float m_rhythmWeighting;
    float m_rhythmClipDuration;
    float m_rhythmClipOrigin;
    int m_rhythmClipFrameSize;
    int m_rhythmClipFrames;
    int m_rhythmColumnSize;
    mutable int m_blockSize; // before decimation
    int m_fftSize; // after decimation
    int m_channels;
    int m_processRate;
    int m_frameNo;
    bool m_done;

    static const float m_noRhythm;
    static const float m_allRhythm;

    std::vector<int> m_lastNonEmptyFrame; // per channel
    std::vector<int> m_emptyFrameCount; // per channel

    mutable int m_distanceMatrixOutput;
    mutable int m_distanceVectorOutput;
    mutable int m_sortedVectorOutput;
    mutable int m_meansOutput;
    mutable int m_variancesOutput;
    mutable int m_beatSpectraOutput;

    typedef std::vector<double> FeatureColumn;
    typedef std::vector<FeatureColumn> FeatureMatrix;
    typedef std::vector<FeatureMatrix> FeatureMatrixSet;

    typedef std::deque<FeatureColumn> FeatureColumnQueue;
    typedef std::vector<FeatureColumnQueue> FeatureQueueSet;

    FeatureMatrixSet m_values;
    FeatureQueueSet m_rhythmValues;

    FeatureMatrix calculateTimbral(FeatureSet &returnFeatures);
    FeatureMatrix calculateRhythmic(FeatureSet &returnFeatures);
    double getDistance(const FeatureMatrix &timbral,
                       const FeatureMatrix &rhythmic,
                       int i, int j);
};

#endif

