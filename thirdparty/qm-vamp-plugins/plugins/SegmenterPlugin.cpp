/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
 * SegmenterPlugin.cpp
 *
 * Created by Mark Levy on 24/03/2006.
 * Copyright 2006 Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
 */

#include <iostream>
#include <sstream>

#include "SegmenterPlugin.h"
#include "dsp/segmentation/ClusterMeltSegmenter.h"

using std::string;
using std::vector;
using std::cerr;
using std::endl;
using std::ostringstream;

SegmenterPlugin::SegmenterPlugin(float inputSampleRate) :
    Plugin(inputSampleRate),
    segmenter(0),
    hopsize(0),
    windowsize(0),
    neighbourhoodLimit(4),
    nSegmentTypes(10),
    featureType(feature_types(1))
{
	
}

SegmenterPlugin::~SegmenterPlugin()
{
    delete segmenter;
}

std::string SegmenterPlugin::getIdentifier() const
{
    return "qm-segmenter";
}

std::string SegmenterPlugin::getName() const
{
    return "Segmenter";
}

std::string SegmenterPlugin::getDescription() const
{
    return "Divide the track into a sequence of consistent segments";
}

string
SegmenterPlugin::getMaker() const
{
    return "Queen Mary, University of London";
}

int
SegmenterPlugin::getPluginVersion() const
{
    return 3;
}

string
SegmenterPlugin::getCopyright() const
{
    return "Plugin by Mark Levy.  Copyright (c) 2006-2013 QMUL - All Rights Reserved";
}

bool
SegmenterPlugin::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) return false;

    if (!segmenter) makeSegmenter();

    if (int(stepSize) != hopsize) {
        std::cerr << "SegmenterPlugin::initialise: supplied step size "
                  << stepSize << " differs from required step size " << hopsize
                  << std::endl;
        return false;
    }

    if (int(blockSize) != windowsize) {
        std::cerr << "SegmenterPlugin::initialise: supplied block size "
                  << blockSize << " differs from required block size " << windowsize
                  << std::endl;
        return false;
    }        
		
    return true;
}

void
SegmenterPlugin::reset()
{
    // re-make segmenter only if it has already been made; otherwise
    // there's nothing to reset
    if (segmenter) makeSegmenter();
}

size_t
SegmenterPlugin::getPreferredStepSize() const
{
    if (!segmenter) makeSegmenter();
    return hopsize;
}

size_t
SegmenterPlugin::getPreferredBlockSize() const
{
    if (!segmenter) makeSegmenter();
    return windowsize;
}

SegmenterPlugin::ParameterList SegmenterPlugin::getParameterDescriptors() const
{
    ParameterList list;
	
    ParameterDescriptor desc;
    desc.identifier = "nSegmentTypes";
    desc.name = "Number of segment-types";
    desc.description = "Maximum number of different kinds of segment to find";
    desc.unit = "";
    desc.minValue = 2;
    desc.maxValue = 12;
    desc.defaultValue = 10;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);
	
    ParameterDescriptor desc2;
    desc2.identifier = "featureType";
    desc2.name = "Feature Type";
    desc2.description = "Try Chromatic for acoustic or pre-1980 recordings, otherwise use Hybrid";
    desc2.unit = "";
    desc2.minValue = 1;
    desc2.maxValue = 3;
    desc2.defaultValue = 1;
    desc2.isQuantized = true;
    desc2.quantizeStep = 1;
    desc2.valueNames.push_back("Hybrid (Constant-Q)");
    desc2.valueNames.push_back("Chromatic (Chroma)");
    desc2.valueNames.push_back("Timbral (MFCC)");
    list.push_back(desc2);	
	
    ParameterDescriptor desc3;
    desc3.identifier = "neighbourhoodLimit";
    desc3.name = "Minimum segment duration";
    desc3.description = "Approximate expected minimum duration for each segment";
    desc3.unit = "s";
    desc3.minValue = 1;
    desc3.maxValue = 15;
    desc3.defaultValue = 4;
    desc3.isQuantized = true;
    desc3.quantizeStep = 0.2;
    list.push_back(desc3);

    return list;
}

float
SegmenterPlugin::getParameter(std::string param) const
{
    if (param == "nSegmentTypes") {
        return nSegmentTypes;
    }
	
    if (param == "featureType") {
        return featureType;
    }

    if (param == "neighbourhoodLimit") {
        return neighbourhoodLimit;
    }
    
    std::cerr << "WARNING: SegmenterPlugin::getParameter: unknown parameter \""
              << param << "\"" << std::endl;
    return 0.0;
}

void
SegmenterPlugin::setParameter(std::string param, float value)
{
    if (param == "nSegmentTypes") {

        nSegmentTypes = int(value + 0.0001);
        return;
    }

    if (param == "featureType") {
		int nval = int(value + 0.5);
        if (featureType != feature_types(nval)) { // feature type changed, create a new segmenter
            featureType = feature_types(nval);
            makeSegmenter();
        }
        return;
    }

    if (param == "neighbourhoodLimit") {
        if (neighbourhoodLimit != value) {
            neighbourhoodLimit = value;
            makeSegmenter();
        }
        return;
    }
    
    std::cerr << "WARNING: SegmenterPlugin::setParameter: unknown parameter \""
              << param << "\"" << std::endl;
}

void
SegmenterPlugin::makeSegmenter() const
{
    ClusterMeltSegmenterParams params = ClusterMeltSegmenterParams();
    params.featureType = (feature_types) featureType;

    if (params.featureType == FEATURE_TYPE_CONSTQ)
    {
        params.ncomponents = 20;
    }
    if (params.featureType == FEATURE_TYPE_CHROMA)
    {
        params.hopSize = 0.1;
        params.windowSize = 0.372;
        params.nbins = 12;
        params.histogramLength = 20;
    }
    if (params.featureType == FEATURE_TYPE_MFCC)
    {
        params.ncomponents = 20;
    }
    delete segmenter;

    params.neighbourhoodLimit =
        int(neighbourhoodLimit / params.hopSize + 0.0001);

    segmenter = new ClusterMeltSegmenter(params);
    segmenter->initialise(m_inputSampleRate);
    hopsize = segmenter->getHopsize();
    windowsize = segmenter->getWindowsize();

//    std::cerr << "segmenter window size: " << segmenter->getWindowsize()
//              << std::endl;
}

SegmenterPlugin::OutputList
SegmenterPlugin::getOutputDescriptors() const
{
    OutputList list;
	
    OutputDescriptor segmentation;
    segmentation.identifier = "segmentation";
    segmentation.name = "Segmentation";
    segmentation.description = "Segmentation";
    segmentation.unit = "segment-type";
    segmentation.hasFixedBinCount = true;
    segmentation.binCount = 1;
    segmentation.hasKnownExtents = true;
    segmentation.minValue = 1;
    segmentation.maxValue = nSegmentTypes;
    segmentation.isQuantized = true;
    segmentation.quantizeStep = 1;
    segmentation.sampleType = OutputDescriptor::VariableSampleRate;
    segmentation.sampleRate = m_inputSampleRate / getPreferredStepSize();
    segmentation.hasDuration = true;
	
    list.push_back(segmentation);
    
    return list;
}

SegmenterPlugin::FeatureSet
SegmenterPlugin::process(const float *const *inputBuffers, Vamp::RealTime timestamp)
{
    // convert float* to double*
    double *tempBuffer = new double[windowsize];
    for (int i = 0; i < windowsize; ++i) {
        tempBuffer[i] = inputBuffers[0][i];
    }

    segmenter->extractFeatures(tempBuffer, segmenter->getWindowsize());

    delete [] tempBuffer;

    m_endTime = timestamp;
	
    return FeatureSet();
}

SegmenterPlugin::FeatureSet
SegmenterPlugin::getRemainingFeatures()
{
    segmenter->segment(nSegmentTypes);
    Segmentation segm = segmenter->getSegmentation();
	
    FeatureSet returnFeatures;

    // Map the segment types onto a dense series starting at 1
	
    std::map<int, int> typeMap;
    int nextType = 1;

    for (int i = 0; i < int(segm.segments.size()); ++i) {
        Segment s = segm.segments[i];
        if (typeMap.find(s.type) == typeMap.end()) {
            typeMap[s.type] = nextType;
            ++nextType;
        }
    }

    for (int i = 0; i < int(segm.segments.size()); ++i) {
		
        Segment s = segm.segments[i];
		
        Feature feature;
        feature.hasTimestamp = true;
        feature.timestamp = Vamp::RealTime::frame2RealTime
            (s.start, (int)m_inputSampleRate);
        feature.hasDuration = true;

        if (i + 1 < int(segm.segments.size())) {
            feature.duration = Vamp::RealTime::frame2RealTime
                (segm.segments[i+1].start - s.start, (int)m_inputSampleRate);
        } else {
            feature.duration = m_endTime - feature.timestamp;
        }            

        int type = typeMap[s.type];
		
        vector<float> floatval;
        floatval.push_back(type);
        feature.values = floatval;
		
        ostringstream oss;
        oss << char('A' + type - 1);
        feature.label = oss.str();
		
        returnFeatures[0].push_back(feature);
    }
	
    return returnFeatures;
}
