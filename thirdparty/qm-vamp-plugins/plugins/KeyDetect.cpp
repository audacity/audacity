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

#include "KeyDetect.h"

using std::string;
using std::vector;

#include <cmath>


// Order for circle-of-5ths plotting
static int conversion[24] =
{ 7, 12, 5, 10, 3, 8, 1, 6, 11, 4, 9, 2,
  16, 21, 14, 19, 24, 17, 22, 15, 20, 13, 18, 23 };


KeyDetector::KeyDetector(float inputSampleRate) :
    Plugin(inputSampleRate),
    m_stepSize(0),
    m_blockSize(0),
    m_tuningFrequency(440),
    m_length(10),
    m_getKeyMode(0),
    m_inputFrame(0),
    m_prevKey(-1)
{
}

KeyDetector::~KeyDetector()
{
    delete m_getKeyMode;
    if ( m_inputFrame ) {
        delete [] m_inputFrame;
    }
}

string
KeyDetector::getIdentifier() const
{
    return "qm-keydetector";
}

string
KeyDetector::getName() const
{
    return "Key Detector";
}

string
KeyDetector::getDescription() const
{
    return "Estimate the key of the music";
}

string
KeyDetector::getMaker() const
{
    return "Queen Mary, University of London";
}

int
KeyDetector::getPluginVersion() const
{
    return 6;
}

string
KeyDetector::getCopyright() const
{
    return "Plugin by Katy Noland and Christian Landone.  Copyright (c) 2006-2019 QMUL - All Rights Reserved";
}

KeyDetector::ParameterList
KeyDetector::getParameterDescriptors() const
{
    ParameterList list;

    ParameterDescriptor desc;
    desc.identifier = "tuning";
    desc.name = "Tuning Frequency";
    desc.description = "Frequency of concert A";
    desc.unit = "Hz";
    desc.minValue = 420;
    desc.maxValue = 460;
    desc.defaultValue = 440;
    desc.isQuantized = false;
    list.push_back(desc);
    
    desc.identifier = "length";
    desc.name = "Window Length";
    desc.unit = "chroma frames";
    desc.description = "Number of chroma analysis frames per key estimation";
    desc.minValue = 1;
    desc.maxValue = 30;
    desc.defaultValue = 10;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);

    return list;
}

float
KeyDetector::getParameter(std::string param) const
{
    if (param == "tuning") {
        return m_tuningFrequency;
    }
    if (param == "length") {
        return float(m_length);
    }
    std::cerr << "WARNING: KeyDetector::getParameter: unknown parameter \""
              << param << "\"" << std::endl;
    return 0.0;
}

void
KeyDetector::setParameter(std::string param, float value)
{
    if (param == "tuning") {
        m_tuningFrequency = value;
    } else if (param == "length") {
        m_length = int(value + 0.1);
    } else {
        std::cerr << "WARNING: KeyDetector::setParameter: unknown parameter \""
                  << param << "\"" << std::endl;
    }

    // force recalculate:
    m_stepSize = 0;
    m_blockSize = 0;
}

GetKeyMode::Config
KeyDetector::getConfig() const
{
    GetKeyMode::Config config(m_inputSampleRate, m_tuningFrequency);
    config.hpcpAverage = m_length;
    config.medianAverage = m_length;
    config.frameOverlapFactor = 1;
    config.decimationFactor = 8;
    return config;
}

bool
KeyDetector::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (m_getKeyMode) {
        delete m_getKeyMode;
        m_getKeyMode = 0;
    }

    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) return false;

    m_getKeyMode = new GetKeyMode(getConfig());

    m_stepSize = m_getKeyMode->getHopSize();
    m_blockSize = m_getKeyMode->getBlockSize();

    if (stepSize != m_stepSize || blockSize != m_blockSize) {
        std::cerr << "KeyDetector::initialise: ERROR: step/block sizes "
                  << stepSize << "/" << blockSize << " differ from required "
                  << m_stepSize << "/" << m_blockSize << std::endl;
        delete m_getKeyMode;
        m_getKeyMode = 0;
        return false;
    }

    m_inputFrame = new double[m_blockSize];

    m_prevKey = -1;
    m_first = true;

    return true;
}

void
KeyDetector::reset()
{
    if (m_getKeyMode) {
        delete m_getKeyMode;
        m_getKeyMode = new GetKeyMode(getConfig());
    }

    if (m_inputFrame) {
        for( unsigned int i = 0; i < m_blockSize; i++ ) {
            m_inputFrame[ i ] = 0.0;
        }
    }

    m_prevKey = -1;
    m_first = true;
}


KeyDetector::OutputList
KeyDetector::getOutputDescriptors() const
{
    OutputList list;

    float osr = 0.0f;
    if (m_stepSize == 0) (void)getPreferredStepSize();
    osr = m_inputSampleRate / float(m_stepSize);

    OutputDescriptor d;
    d.identifier = "tonic";
    d.name = "Tonic Pitch";
    d.unit = "";
    d.description = "Tonic of the estimated key (from C = 1 to B = 12)";
    d.hasFixedBinCount = true;
    d.binCount = 1;
    d.hasKnownExtents = true;
    d.isQuantized = true;
    d.minValue = 1;
    d.maxValue = 12;
    d.quantizeStep = 1;
    d.sampleRate = osr;
    d.sampleType = OutputDescriptor::VariableSampleRate;
    list.push_back(d);

    d.identifier = "mode";
    d.name = "Key Mode";
    d.unit = "";
    d.description = "Major or minor mode of the estimated key (major = 0, minor = 1)";
    d.hasFixedBinCount = true;
    d.binCount = 1;
    d.hasKnownExtents = true;
    d.isQuantized = true;
    d.minValue = 0;
    d.maxValue = 1;
    d.quantizeStep = 1;
    d.sampleRate = osr;
    d.sampleType = OutputDescriptor::VariableSampleRate;
    list.push_back(d);

    d.identifier = "key";
    d.name = "Key";
    d.unit = "";
    d.description = "Estimated key (from C major = 1 to B major = 12 and C minor = 13 to B minor = 24)";
    d.hasFixedBinCount = true;
    d.binCount = 1;
    d.hasKnownExtents = true;
    d.isQuantized = true;
    d.minValue = 1;
    d.maxValue = 24;
    d.quantizeStep = 1;
    d.sampleRate = osr;
    d.sampleType = OutputDescriptor::VariableSampleRate;
    list.push_back(d);

    d.identifier = "keystrength";
    d.name = "Key Strength Plot";
    d.unit = "";
    d.description = "Correlation of the chroma vector with stored key profile for each major and minor key";
    d.hasFixedBinCount = true;
    d.binCount = 25;
    d.hasKnownExtents = false;
    d.isQuantized = false;
    d.sampleType = OutputDescriptor::OneSamplePerStep;
    d.binNames.clear();
    for (int i = 0; i < 24; ++i) {
        if (i == 12) d.binNames.push_back(" ");
        int idx = conversion[i];
        std::string label = getKeyName(idx > 12 ? idx-12 : idx, 
                                       i >= 12,
                                       true);
        d.binNames.push_back(label);
    }
    list.push_back(d);

    d.identifier = "mergedkeystrength";
    d.name = "Merged Key Strength Plot";
    d.unit = "";
    d.description = "Correlation of the chroma vector with stored key profile for each key, with major and minor alternatives merged";
    d.hasFixedBinCount = true;
    d.binCount = 12;
    d.hasKnownExtents = false;
    d.isQuantized = false;
    d.sampleType = OutputDescriptor::OneSamplePerStep;
    d.binNames.clear();
    for (int i = 0; i < 12; ++i) {
        int idx = conversion[i];
        std::string label = getBothKeyNames(idx > 12 ? idx-12 : idx);
        d.binNames.push_back(label);
    }
    list.push_back(d);

    return list;
}

KeyDetector::FeatureSet
KeyDetector::process(const float *const *inputBuffers,
                     Vamp::RealTime now)
{
    if (m_stepSize == 0) {
	return FeatureSet();
    }

    FeatureSet returnFeatures;

    for ( unsigned int i = 0 ; i < m_blockSize; i++ ) {
        m_inputFrame[i] = (double)inputBuffers[0][i];
    }

    int key = m_getKeyMode->process(m_inputFrame);

    int tonic = key;
    if (tonic > 12) tonic -= 12;

    int prevTonic = m_prevKey;
    if (prevTonic > 12) prevTonic -= 12;

    bool minor = (key > 12);
    bool prevMinor = (m_prevKey > 12);

    if (m_first || (tonic != prevTonic)) {
        Feature feature;
        feature.hasTimestamp = true;
        feature.timestamp = now;
        feature.values.push_back((float)tonic);
        feature.label = getKeyName(tonic, minor, false);
        returnFeatures[0].push_back(feature); // tonic
    }

    if (m_first || (minor != prevMinor)) {
        Feature feature;
        feature.hasTimestamp = true;
        feature.timestamp = now;
        feature.values.push_back(minor ? 1.f : 0.f);
        feature.label = (minor ? "Minor" : "Major");
        returnFeatures[1].push_back(feature); // mode
    }

    if (m_first || (key != m_prevKey)) {
        Feature feature;
        feature.hasTimestamp = true;
        feature.timestamp = now;
        feature.values.push_back((float)key);
        feature.label = getKeyName(tonic, minor, true);
        returnFeatures[2].push_back(feature); // key
    }

    m_prevKey = key;
    m_first = false;

    Feature ksf;
    ksf.hasTimestamp = false;
    ksf.values.reserve(25);

    Feature tsf;
    tsf.hasTimestamp = false;
    tsf.values.reserve(12);

    double *keystrengths = m_getKeyMode->getKeyStrengths();

    for (int i = 0; i < 24; ++i) {

        if (i == 12) ksf.values.push_back(-1);
        ksf.values.push_back(float(keystrengths[conversion[i]-1]));

        if (i < 12) {
            tsf.values.push_back(float(keystrengths[conversion[i]-1]));
        } else {
            tsf.values[i-12] += float(keystrengths[conversion[i]-1]);
        }
    }
    
    returnFeatures[3].push_back(ksf);
    returnFeatures[4].push_back(tsf);

    return returnFeatures;
}

KeyDetector::FeatureSet
KeyDetector::getRemainingFeatures()
{
    return FeatureSet();
}

size_t
KeyDetector::getPreferredStepSize() const
{
    if (!m_stepSize) {
        GetKeyMode gkm(getConfig());
        m_stepSize = gkm.getHopSize();
        m_blockSize = gkm.getBlockSize();
    }
    return m_stepSize;
}

size_t
KeyDetector::getPreferredBlockSize() const
{
    if (!m_blockSize) {
        GetKeyMode gkm(getConfig());
        m_stepSize = gkm.getHopSize();
        m_blockSize = gkm.getBlockSize();
    }
    return m_blockSize;
}

std::string
KeyDetector::getKeyName(int index, bool minor, bool includeMajMin) const
{
    // Keys are numbered with 1 => C, 12 => B
    // This is based on chromagram base set to a C in qm-dsp's GetKeyMode.cpp

    static const char *namesMajor[] = {
        "C", "Db", "D", "Eb",
        "E", "F", "F# / Gb", "G",
        "Ab", "A", "Bb", "B"
    };

    static const char *namesMinor[] = {
        "C", "C#", "D", "Eb / D#",
        "E", "F", "F#", "G",
        "G#", "A", "Bb", "B"
    };

    if (index < 1 || index > 12) {
        return "(unknown)";
    }

    std::string base;

    if (minor) base = namesMinor[index - 1];
    else base = namesMajor[index - 1];

    if (!includeMajMin) return base;

    if (minor) return base + " minor";
    else return base + " major";
}

std::string
KeyDetector::getBothKeyNames(int index) const
{
    // Keys are numbered with 1 => C, 12 => B

    static const char *names[] = {
        "C maj / A min",
        "Db maj / Bb min",
        "D maj / B min",
        "Eb maj / C min",
        "E maj / C# min",
        "F maj / D min",
        "F#/Gb maj / Eb/D# min",
        "G maj / E min",
        "Ab maj / F min",
        "A maj / F# min",
        "Bb maj / G min",
        "B maj / G# min"
    };

    if (index < 1 || index > 12) {
        return "(unknown)";
    }

    return names[index - 1];
}
    

