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

#include "BeatTrack.h"

#include <dsp/onsets/DetectionFunction.h>
#include <dsp/onsets/PeakPicking.h>
#include <dsp/tempotracking/TempoTrack.h>
#include <dsp/tempotracking/TempoTrackV2.h>

using std::string;
using std::vector;
using std::cerr;
using std::endl;

float BeatTracker::m_stepSecs = 0.01161; // 512 samples at 44100

#define METHOD_OLD 0
#define METHOD_NEW 1

class BeatTrackerData
{
public:
    BeatTrackerData(const DFConfig &config) : dfConfig(config) {
    df = new DetectionFunction(config);
    }
    ~BeatTrackerData() {
    delete df;
    }
    void reset() {
    delete df;
    df = new DetectionFunction(dfConfig);
    dfOutput.clear();
        origin = Vamp::RealTime::zeroTime;
    }

    DFConfig dfConfig;
    DetectionFunction *df;
    vector<double> dfOutput;
    Vamp::RealTime origin;
};


BeatTracker::BeatTracker(float inputSampleRate) :
    Vamp::Plugin(inputSampleRate),
    m_d(0),
    m_method(METHOD_NEW),
    m_dfType(DF_COMPLEXSD),
    m_alpha(0.9),  			// MEPD new exposed parameter for beat tracker, default value = 0.9 (as old version)
    m_tightness(4.),
    m_inputtempo(120.), 	// MEPD new exposed parameter for beat tracker, default value = 120. (as old version)
    m_constraintempo(false), // MEPD new exposed parameter for beat tracker, default value = false (as old version)
    // calling the beat tracker with these default parameters will give the same output as the previous existing version
    m_whiten(false)

{
}

BeatTracker::~BeatTracker()
{
    delete m_d;
}

string
BeatTracker::getIdentifier() const
{
    return "qm-tempotracker";
}

string
BeatTracker::getName() const
{
    return "Tempo and Beat Tracker";
}

string
BeatTracker::getDescription() const
{
    return "Estimate beat locations and tempo";
}

string
BeatTracker::getMaker() const
{
    return "Queen Mary, University of London";
}

int
BeatTracker::getPluginVersion() const
{
    return 6;
}

string
BeatTracker::getCopyright() const
{
    return "Plugin by Christian Landone and Matthew Davies.  Copyright (c) 2006-2013 QMUL - All Rights Reserved";
}

BeatTracker::ParameterList
BeatTracker::getParameterDescriptors() const
{
    ParameterList list;

    ParameterDescriptor desc;

    desc.identifier = "method";
    desc.name = "Beat Tracking Method";
    desc.description = "Basic method to use ";
    desc.minValue = 0;
    desc.maxValue = 1;
    desc.defaultValue = METHOD_NEW;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    desc.valueNames.push_back("Old");
    desc.valueNames.push_back("New");
    list.push_back(desc);

    desc.identifier = "dftype";
    desc.name = "Onset Detection Function Type";
    desc.description = "Method used to calculate the onset detection function";
    desc.minValue = 0;
    desc.maxValue = 4;
    desc.defaultValue = 3;
    desc.valueNames.clear();
    desc.valueNames.push_back("High-Frequency Content");
    desc.valueNames.push_back("Spectral Difference");
    desc.valueNames.push_back("Phase Deviation");
    desc.valueNames.push_back("Complex Domain");
    desc.valueNames.push_back("Broadband Energy Rise");
    list.push_back(desc);

    desc.identifier = "whiten";
    desc.name = "Adaptive Whitening";
    desc.description = "Normalize frequency bin magnitudes relative to recent peak levels";
    desc.minValue = 0;
    desc.maxValue = 1;
    desc.defaultValue = 0;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    desc.unit = "";
    desc.valueNames.clear();
    list.push_back(desc);

    // MEPD new exposed parameter - used in the dynamic programming part of the beat tracker
    //Alpha Parameter of Beat Tracker
    desc.identifier = "alpha";
    desc.name = "Alpha";
    desc.description = "Inertia - Flexibility Trade Off";
    desc.minValue =  0.1;
    desc.maxValue = 0.99;
    desc.defaultValue = 0.90;
    desc.unit = "";
    desc.isQuantized = false;
    list.push_back(desc);

    // We aren't exposing tightness as a parameter, it's fixed at 4

    // MEPD new exposed parameter - used in the periodicity estimation
    //User input tempo
    desc.identifier = "inputtempo";
    desc.name = "Tempo Hint";
    desc.description = "User-defined tempo on which to centre the tempo preference function";
    desc.minValue =  50;
    desc.maxValue = 250;
    desc.defaultValue = 120;
    desc.unit = "BPM";
    desc.isQuantized = true;
    list.push_back(desc);

    // MEPD new exposed parameter - used in periodicity estimation
    desc.identifier = "constraintempo";
    desc.name = "Constrain Tempo";
    desc.description = "Constrain more tightly around the tempo hint, using a Gaussian weighting instead of Rayleigh";
    desc.minValue = 0;
    desc.maxValue = 1;
    desc.defaultValue = 0;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    desc.unit = "";
    desc.valueNames.clear();
    list.push_back(desc);



    return list;
}

float
BeatTracker::getParameter(std::string name) const
{
    if (name == "dftype") {
        switch (m_dfType) {
        case DF_HFC: return 0;
        case DF_SPECDIFF: return 1;
        case DF_PHASEDEV: return 2;
        default: case DF_COMPLEXSD: return 3;
        case DF_BROADBAND: return 4;
        }
    } else if (name == "method") {
        return m_method;
    } else if (name == "whiten") {
        return m_whiten ? 1.0 : 0.0;
    } else if (name == "alpha") {
        return m_alpha;
    }  else if (name == "inputtempo") {
        return m_inputtempo;
    }  else if (name == "constraintempo") {
        return m_constraintempo ? 1.0 : 0.0;
    }
    return 0.0;
}

void
BeatTracker::setParameter(std::string name, float value)
{
    if (name == "dftype") {
        switch (lrintf(value)) {
        case 0: m_dfType = DF_HFC; break;
        case 1: m_dfType = DF_SPECDIFF; break;
        case 2: m_dfType = DF_PHASEDEV; break;
        default: case 3: m_dfType = DF_COMPLEXSD; break;
        case 4: m_dfType = DF_BROADBAND; break;
        }
    } else if (name == "method") {
        m_method = lrintf(value);
    } else if (name == "whiten") {
        m_whiten = (value > 0.5);
    } else if (name == "alpha") {
        m_alpha = value;
    } else if (name == "inputtempo") {
        m_inputtempo = value;
    } else if (name == "constraintempo") {
        m_constraintempo = (value > 0.5);
    }
}

bool
BeatTracker::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (m_d) {
    delete m_d;
    m_d = 0;
    }

    if (channels < getMinChannelCount() ||
    channels > getMaxChannelCount()) {
        std::cerr << "BeatTracker::initialise: Unsupported channel count: "
                  << channels << std::endl;
        return false;
    }

    if (stepSize != getPreferredStepSize()) {
        std::cerr << "ERROR: BeatTracker::initialise: Unsupported step size for this sample rate: "
                  << stepSize << " (wanted " << (getPreferredStepSize()) << ")" << std::endl;
        return false;
    }

    if (blockSize != getPreferredBlockSize()) {
        std::cerr << "WARNING: BeatTracker::initialise: Sub-optimal block size for this sample rate: "
                  << blockSize << " (wanted " << getPreferredBlockSize() << ")" << std::endl;
//        return false;
    }

    DFConfig dfConfig;
    dfConfig.DFType = m_dfType;
    dfConfig.stepSize = stepSize;
    dfConfig.frameLength = blockSize;
    dfConfig.dbRise = 3;
    dfConfig.adaptiveWhitening = m_whiten;
    dfConfig.whiteningRelaxCoeff = -1;
    dfConfig.whiteningFloor = -1;

    m_d = new BeatTrackerData(dfConfig);
    return true;
}

void
BeatTracker::reset()
{
    if (m_d) m_d->reset();
}

size_t
BeatTracker::getPreferredStepSize() const
{
    size_t step = size_t(m_inputSampleRate * m_stepSecs + 0.0001);
//    std::cerr << "BeatTracker::getPreferredStepSize: input sample rate is " << m_inputSampleRate << ", step size is " << step << std::endl;
    return step;
}

size_t
BeatTracker::getPreferredBlockSize() const
{
    size_t theoretical = getPreferredStepSize() * 2;

    // I think this is not necessarily going to be a power of two, and
    // the host might have a problem with that, but I'm not sure we
    // can do much about it here
    return theoretical;
}

BeatTracker::OutputList
BeatTracker::getOutputDescriptors() const
{
    OutputList list;

    OutputDescriptor beat;
    beat.identifier = "beats";
    beat.name = "Beats";
    beat.description = "Estimated metrical beat locations";
    beat.unit = "";
    beat.hasFixedBinCount = true;
    beat.binCount = 0;
    beat.sampleType = OutputDescriptor::VariableSampleRate;
    beat.sampleRate = 1.0 / m_stepSecs;

    OutputDescriptor df;
    df.identifier = "detection_fn";
    df.name = "Onset Detection Function";
    df.description = "Probability function of note onset likelihood";
    df.unit = "";
    df.hasFixedBinCount = true;
    df.binCount = 1;
    df.hasKnownExtents = false;
    df.isQuantized = false;
    df.sampleType = OutputDescriptor::OneSamplePerStep;

    OutputDescriptor tempo;
    tempo.identifier = "tempo";
    tempo.name = "Tempo";
    tempo.description = "Locked tempo estimates";
    tempo.unit = "bpm";
    tempo.hasFixedBinCount = true;
    tempo.binCount = 1;
    tempo.hasKnownExtents = false;
    tempo.isQuantized = false;
    tempo.sampleType = OutputDescriptor::VariableSampleRate;
    tempo.sampleRate = 1.0 / m_stepSecs;

    list.push_back(beat);
    list.push_back(df);
    list.push_back(tempo);

    return list;
}

BeatTracker::FeatureSet
BeatTracker::process(const float *const *inputBuffers,
                     Vamp::RealTime timestamp)
{
    if (!m_d) {
    cerr << "ERROR: BeatTracker::process: "
         << "BeatTracker has not been initialised"
         << endl;
    return FeatureSet();
    }

    size_t len = m_d->dfConfig.frameLength / 2 + 1;

    double *reals = new double[len];
    double *imags = new double[len];

    // We only support a single input channel

    for (size_t i = 0; i < len; ++i) {
        reals[i] = inputBuffers[0][i*2];
        imags[i] = inputBuffers[0][i*2+1];
    }

    double output = m_d->df->processFrequencyDomain(reals, imags);

    delete[] reals;
    delete[] imags;

    if (m_d->dfOutput.empty()) m_d->origin = timestamp;

    m_d->dfOutput.push_back(output);

    FeatureSet returnFeatures;

    Feature feature;
    feature.hasTimestamp = false;
    feature.values.push_back(output);

    returnFeatures[1].push_back(feature); // detection function is output 1
    return returnFeatures;
}

BeatTracker::FeatureSet
BeatTracker::getRemainingFeatures()
{
    if (!m_d) {
    cerr << "ERROR: BeatTracker::getRemainingFeatures: "
         << "BeatTracker has not been initialised"
         << endl;
    return FeatureSet();
    }

    if (m_method == METHOD_OLD) return beatTrackOld();
    else return beatTrackNew();
}

BeatTracker::FeatureSet
BeatTracker::beatTrackOld()
{
    double aCoeffs[] = { 1.0000, -0.5949, 0.2348 };
    double bCoeffs[] = { 0.1600,  0.3200, 0.1600 };

    TTParams ttParams;
    ttParams.winLength = 512;
    ttParams.lagLength = 128;
    ttParams.LPOrd = 2;
    ttParams.LPACoeffs = aCoeffs;
    ttParams.LPBCoeffs = bCoeffs;
    ttParams.alpha = 9;
    ttParams.WinT.post = 8;
    ttParams.WinT.pre = 7;

    TempoTrack tempoTracker(ttParams);

    vector<double> tempi;
    vector<int> beats = tempoTracker.process(m_d->dfOutput, &tempi);

    FeatureSet returnFeatures;

    char label[100];

    for (size_t i = 0; i < beats.size(); ++i) {

    size_t frame = beats[i] * m_d->dfConfig.stepSize;

    Feature feature;
    feature.hasTimestamp = true;
    feature.timestamp = m_d->origin + Vamp::RealTime::frame2RealTime
        (frame, lrintf(m_inputSampleRate));

    float bpm = 0.0;
    int frameIncrement = 0;

    if (i < beats.size() - 1) {

        frameIncrement = (beats[i+1] - beats[i]) * m_d->dfConfig.stepSize;

        // one beat is frameIncrement frames, so there are
        // samplerate/frameIncrement bps, so
        // 60*samplerate/frameIncrement bpm

        if (frameIncrement > 0) {
        bpm = (60.0 * m_inputSampleRate) / frameIncrement;
        bpm = int(bpm * 100.0 + 0.5) / 100.0;
                sprintf(label, "%.2f bpm", bpm);
                feature.label = label;
        }
    }

    returnFeatures[0].push_back(feature); // beats are output 0
    }

    double prevTempo = 0.0;

    for (size_t i = 0; i < tempi.size(); ++i) {

        size_t frame = i * m_d->dfConfig.stepSize * ttParams.lagLength;

//        std::cerr << "unit " << i << ", step size " << m_d->dfConfig.stepSize << ", hop " << ttParams.lagLength << ", frame = " << frame << std::endl;

        if (tempi[i] > 1 && int(tempi[i] * 100) != int(prevTempo * 100)) {
            Feature feature;
            feature.hasTimestamp = true;
            feature.timestamp = m_d->origin + Vamp::RealTime::frame2RealTime
                (frame, lrintf(m_inputSampleRate));
            feature.values.push_back(tempi[i]);
            sprintf(label, "%.2f bpm", tempi[i]);
            feature.label = label;
            returnFeatures[2].push_back(feature); // tempo is output 2
            prevTempo = tempi[i];
        }
    }

    return returnFeatures;
}

BeatTracker::FeatureSet
BeatTracker::beatTrackNew()
{
    vector<double> df;
    vector<double> beatPeriod;
    vector<double> tempi;

    size_t nonZeroCount = m_d->dfOutput.size();
    while (nonZeroCount > 0) {
        if (m_d->dfOutput[nonZeroCount-1] > 0.0) {
            break;
        }
        --nonZeroCount;
    }

//    std::cerr << "Note: nonZeroCount was " << m_d->dfOutput.size() << ", is now " << nonZeroCount << std::endl;

    for (size_t i = 2; i < nonZeroCount; ++i) { // discard first two elts
        df.push_back(m_d->dfOutput[i]);
        beatPeriod.push_back(0.0);
    }
    if (df.empty()) return FeatureSet();

    TempoTrackV2 tt(m_inputSampleRate, m_d->dfConfig.stepSize);


    // MEPD - note this function is now passed 2 new parameters, m_inputtempo and m_constraintempo
    tt.calculateBeatPeriod(df, beatPeriod, tempi, m_inputtempo, m_constraintempo);

    vector<double> beats;

    // MEPD - note this function is now passed 2 new parameters, m_alpha and m_tightness
    tt.calculateBeats(df, beatPeriod, beats, m_alpha, m_tightness);

    FeatureSet returnFeatures;

    char label[100];

    for (size_t i = 0; i < beats.size(); ++i) {

    size_t frame = beats[i] * m_d->dfConfig.stepSize;

    Feature feature;
    feature.hasTimestamp = true;
    feature.timestamp = m_d->origin + Vamp::RealTime::frame2RealTime
        (frame, lrintf(m_inputSampleRate));

    float bpm = 0.0;
    int frameIncrement = 0;

    if (i+1 < beats.size()) {

        frameIncrement = (beats[i+1] - beats[i]) * m_d->dfConfig.stepSize;

        // one beat is frameIncrement frames, so there are
        // samplerate/frameIncrement bps, so
        // 60*samplerate/frameIncrement bpm

        if (frameIncrement > 0) {
        bpm = (60.0 * m_inputSampleRate) / frameIncrement;
        bpm = int(bpm * 100.0 + 0.5) / 100.0;
                sprintf(label, "%.2f bpm", bpm);
                feature.label = label;
        }
    }

    returnFeatures[0].push_back(feature); // beats are output 0
    }

    double prevTempo = 0.0;

    for (size_t i = 0; i < tempi.size(); ++i) {

    size_t frame = i * m_d->dfConfig.stepSize;

        if (tempi[i] > 1 && int(tempi[i] * 100) != int(prevTempo * 100)) {
            Feature feature;
            feature.hasTimestamp = true;
            feature.timestamp = m_d->origin + Vamp::RealTime::frame2RealTime
                (frame, lrintf(m_inputSampleRate));
            feature.values.push_back(tempi[i]);
            sprintf(label, "%.2f bpm", tempi[i]);
            feature.label = label;
            returnFeatures[2].push_back(feature); // tempo is output 2
            prevTempo = tempi[i];
        }
    }

    return returnFeatures;
}
