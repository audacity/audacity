/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM Vamp Plugin Set

    Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#include "TonalChangeDetect.h"

#include <base/Pitch.h>
#include <dsp/chromagram/Chromagram.h>
#include <dsp/tonal/ChangeDetectionFunction.h>

using std::cerr;
using std::endl;

TonalChangeDetect::TonalChangeDetect(float fInputSampleRate)	
    : Vamp::Plugin(fInputSampleRate),
      m_chromagram(0),
      m_step(0),
      m_block(0),
      m_stepDelay(0),
      m_origin(Vamp::RealTime::zeroTime),
      m_haveOrigin(false)
{
    m_minMIDIPitch = 32;
    m_maxMIDIPitch = 108;
    m_tuningFrequency = 440;	
    m_iSmoothingWidth = 5;

    setupConfig();
}

TonalChangeDetect::~TonalChangeDetect()
{
}

bool TonalChangeDetect::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (m_chromagram) {
        delete m_chromagram;
        m_chromagram = 0;
    }
	
    if (channels < getMinChannelCount() ||
        channels > getMaxChannelCount()) {
        std::cerr << "TonalChangeDetect::initialise: Given channel count " << channels << " outside acceptable range (" << getMinChannelCount() << " to " << getMaxChannelCount() << ")" << std::endl;
        return false;
    }

    m_chromagram = new Chromagram(m_config);
    m_step = m_chromagram->getHopSize();
    m_block = m_chromagram->getFrameSize();

    if (stepSize != m_step) {
        std::cerr << "TonalChangeDetect::initialise: Given step size " << stepSize << " differs from only acceptable value " << m_step << std::endl;
        delete m_chromagram;
        m_chromagram = 0;
        return false;
    }
    if (blockSize != m_block) {
        std::cerr << "TonalChangeDetect::initialise: Given step size " << stepSize << " differs from only acceptable value " << m_step << std::endl;
        delete m_chromagram;
        m_chromagram = 0;
        return false;
    }
	
    //    m_stepDelay = (blockSize - stepSize) / 2;
    //    m_stepDelay = m_stepDelay / stepSize;
    m_stepDelay = (blockSize - stepSize) / stepSize; //!!! why? seems about right to look at, but...
	
//    std::cerr << "TonalChangeDetect::initialise: step " << stepSize << ", block "
//              << blockSize << ", delay " << m_stepDelay << std::endl;
	
    m_vaCurrentVector.resize(12, 0.0);
	
    return true;
	
}

std::string TonalChangeDetect::getIdentifier() const
{
    return "qm-tonalchange";
}

std::string TonalChangeDetect::getName() const
{
    return "Tonal Change";
}

std::string TonalChangeDetect::getDescription() const
{
    return "Detect and return the positions of harmonic changes such as chord boundaries";
}

std::string TonalChangeDetect::getMaker() const
{
    return "Queen Mary, University of London";
}

int TonalChangeDetect::getPluginVersion() const
{
    return 4;
}

std::string TonalChangeDetect::getCopyright() const
{
    return "Plugin by Martin Gasser and Christopher Harte.  Copyright (c) 2006-2009 QMUL - All Rights Reserved";
}

TonalChangeDetect::ParameterList TonalChangeDetect::getParameterDescriptors() const
{
    ParameterList list;

    ParameterDescriptor desc;
    desc.identifier = "smoothingwidth";
    desc.name = "Gaussian smoothing";
    desc.description = "Window length for the internal smoothing operation, in chroma analysis frames";
    desc.unit = "frames";
    desc.minValue = 0;
    desc.maxValue = 20;
    desc.defaultValue = 5;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);

    desc.identifier = "minpitch";
    desc.name = "Chromagram minimum pitch";
    desc.unit = "MIDI units";
    desc.description = "Lowest pitch in MIDI units to be included in the chroma analysis";
    desc.minValue = 0;
    desc.maxValue = 127;
    desc.defaultValue = 32;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);

    desc.identifier = "maxpitch";
    desc.name = "Chromagram maximum pitch";
    desc.unit = "MIDI units";
    desc.description = "Highest pitch in MIDI units to be included in the chroma analysis";
    desc.minValue = 0;
    desc.maxValue = 127;
    desc.defaultValue = 108;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);

    desc.identifier = "tuning";
    desc.name = "Chromagram tuning frequency";
    desc.unit = "Hz";
    desc.description = "Frequency of concert A in the music under analysis";
    desc.minValue = 420;
    desc.maxValue = 460;
    desc.defaultValue = 440;
    desc.isQuantized = false;
    list.push_back(desc);

    return list;
}

float
TonalChangeDetect::getParameter(std::string param) const
{
    if (param == "smoothingwidth") {
        return m_iSmoothingWidth;
    }
    if (param == "minpitch") {
        return m_minMIDIPitch;
    }
    if (param == "maxpitch") {
        return m_maxMIDIPitch;
    }
    if (param == "tuning") {
        return m_tuningFrequency;
    }

    std::cerr << "WARNING: ChromagramPlugin::getParameter: unknown parameter \""
              << param << "\"" << std::endl;
    return 0.0;
}

void
TonalChangeDetect::setParameter(std::string param, float value)
{
    if (param == "minpitch") {
        m_minMIDIPitch = lrintf(value);
    } else if (param == "maxpitch") {
        m_maxMIDIPitch = lrintf(value);
    } else if (param == "tuning") {
        m_tuningFrequency = value;
    }
    else if (param == "smoothingwidth") {
        m_iSmoothingWidth = int(value);
    } else {
        std::cerr << "WARNING: ChromagramPlugin::setParameter: unknown parameter \""
                  << param << "\"" << std::endl;
    }

    setupConfig();
}


void TonalChangeDetect::setupConfig()
{
    m_config.FS = lrintf(m_inputSampleRate);
    m_config.min = Pitch::getFrequencyForPitch
        (m_minMIDIPitch, 0, m_tuningFrequency);
    m_config.max = Pitch::getFrequencyForPitch
        (m_maxMIDIPitch, 0, m_tuningFrequency);
    m_config.BPO = 12;
    m_config.CQThresh = 0.0054;
    m_config.normalise = MathUtilities::NormaliseNone;

    m_step = 0;
    m_block = 0;
	
	
}

void
TonalChangeDetect::reset()
{
    if (m_chromagram) {
        delete m_chromagram;
        m_chromagram = new Chromagram(m_config);
    }
    while (!m_pending.empty()) m_pending.pop();
    m_vaCurrentVector.clear();
    m_TCSGram.clear();

    m_origin = Vamp::RealTime::zeroTime;
    m_haveOrigin = false;
}

size_t
TonalChangeDetect::getPreferredStepSize() const
{
    if (!m_step) {
	Chromagram chroma(m_config);
	m_step = chroma.getHopSize();
	m_block = chroma.getFrameSize();
    }

    return m_step;
}

size_t
TonalChangeDetect::getPreferredBlockSize() const
{
    if (!m_step) {
	Chromagram chroma(m_config);
	m_step = chroma.getHopSize();
	m_block = chroma.getFrameSize();
    }

    return m_block;
}

TonalChangeDetect::OutputList TonalChangeDetect::getOutputDescriptors() const
{
    OutputList list;
	 
    OutputDescriptor hc;
    hc.identifier = "tcstransform";
    hc.name = "Transform to 6D Tonal Content Space";
    hc.unit = "";
    hc.description = "Representation of content in a six-dimensional tonal space";
    hc.hasFixedBinCount = true;
    hc.binCount = 6;
    hc.hasKnownExtents = true;
    hc.minValue = -1.0;
    hc.maxValue = 1.0;
    hc.isQuantized = false;
    hc.sampleType = OutputDescriptor::OneSamplePerStep;

    OutputDescriptor d;
    d.identifier = "tcfunction";
    d.name = "Tonal Change Detection Function";
    d.unit = "";
    d.description = "Estimate of the likelihood of a tonal change occurring within each spectral frame";
    d.minValue = 0;
    d.minValue = 2;
    d.hasFixedBinCount = true;
    d.binCount = 1;
    d.hasKnownExtents = false;
    d.isQuantized = false;
    d.sampleType = OutputDescriptor::VariableSampleRate;
    double dStepSecs = double(getPreferredStepSize()) / m_inputSampleRate;
    d.sampleRate = 1.0f / dStepSecs;
	
    OutputDescriptor changes;
    changes.identifier = "changepositions";
    changes.name = "Tonal Change Positions";
    changes.unit = "";
    changes.description = "Estimated locations of tonal changes";
    changes.hasFixedBinCount = true;
    changes.binCount = 0;
    changes.hasKnownExtents = false;
    changes.isQuantized = false;
    changes.sampleType = OutputDescriptor::VariableSampleRate;
    changes.sampleRate = 1.0 / dStepSecs;
	
    list.push_back(hc);
    list.push_back(d);
    list.push_back(changes);

    return list;
}

TonalChangeDetect::FeatureSet
TonalChangeDetect::process(const float *const *inputBuffers,

                           Vamp::RealTime timestamp)
{
    if (!m_chromagram) {
	cerr << "ERROR: TonalChangeDetect::process: "
	     << "Chromagram has not been initialised"
	     << endl;
	return FeatureSet();
    }

    if (!m_haveOrigin) {
        m_origin = timestamp;
        m_haveOrigin = true;
    }

    // convert float* to double*
    double *tempBuffer = new double[m_block];
    for (size_t i = 0; i < m_block; ++i) {
        tempBuffer[i] = inputBuffers[0][i];
    }

    double *output = m_chromagram->process(tempBuffer);
    delete[] tempBuffer;

    for (size_t i = 0; i < 12; i++)
    {
        m_vaCurrentVector[i] = output[i];
    }
	
	
    FeatureSet returnFeatures;

    if (m_stepDelay == 0) {
        m_vaCurrentVector.normalizeL1();
        TCSVector tcsVector = m_TonalEstimator.transform2TCS(m_vaCurrentVector);
        m_TCSGram.addTCSVector(tcsVector);
	
        Feature feature;
        feature.hasTimestamp = false;
        for (int i = 0; i < 6; i++)
        { feature.values.push_back(static_cast<float>(tcsVector[i])); }
        feature.label = "";
        returnFeatures[0].push_back(feature);

        return returnFeatures;
    }
	
    if (m_pending.size() == m_stepDelay) {
	
        ChromaVector v = m_pending.front();
        v.normalizeL1();
        TCSVector tcsVector = m_TonalEstimator.transform2TCS(v);
        m_TCSGram.addTCSVector(tcsVector);
	
        Feature feature;
        feature.hasTimestamp = false;
        for (int i = 0; i < 6; i++)
        { feature.values.push_back(static_cast<float>(tcsVector[i])); }
        feature.label = "";
        returnFeatures[0].push_back(feature);
        m_pending.pop();
		
    } else {
        returnFeatures[0].push_back(Feature());
        m_TCSGram.addTCSVector(TCSVector());
    }

    m_pending.push(m_vaCurrentVector);
	

    return returnFeatures;
}

TonalChangeDetect::FeatureSet TonalChangeDetect::getRemainingFeatures()
{
    FeatureSet returnFeatures;

    while (!m_pending.empty()) {
        ChromaVector v = m_pending.front();
        v.normalizeL1();
        TCSVector tcsVector = m_TonalEstimator.transform2TCS(v);
        m_TCSGram.addTCSVector(tcsVector);
	
        Feature feature;
        feature.hasTimestamp = false;
        for (int i = 0; i < 6; i++)
        { feature.values.push_back(static_cast<float>(tcsVector[i])); }
        feature.label = "";
        returnFeatures[0].push_back(feature);
        m_pending.pop();
    }
	
    ChangeDFConfig dfc;
    dfc.smoothingWidth = double(m_iSmoothingWidth);
    ChangeDetectionFunction df(dfc);
    ChangeDistance d = df.process(m_TCSGram);
	
    for (int i = 0; i < int(d.size()); i++)
    {
        double dCurrent = d[i];
        double dPrevious = d[i > 0 ? i - 1 : i];
        double dNext = d[i < int(d.size())-1 ? i + 1 : i];
		
        Feature feature;
        feature.label = "";
        feature.hasTimestamp = true;
        feature.timestamp = m_origin +
            Vamp::RealTime::frame2RealTime(i*m_step, m_inputSampleRate);
        feature.values.push_back(dCurrent);
        returnFeatures[1].push_back(feature);


        if (dCurrent > dPrevious && dCurrent > dNext)
        {
            Feature featurePeak;
            featurePeak.label = "";
            featurePeak.hasTimestamp = true;
            featurePeak.timestamp = m_origin +
                Vamp::RealTime::frame2RealTime(i*m_step, m_inputSampleRate);
            returnFeatures[2].push_back(featurePeak);
        }

    }

	
    return returnFeatures;
	
}

