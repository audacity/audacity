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

#include "OnsetDetect.h"

#include <dsp/onsets/DetectionFunction.h>
#include <dsp/onsets/PeakPicking.h>
#include <dsp/tempotracking/TempoTrack.h>

using std::string;
using std::vector;
using std::cerr;
using std::endl;

float OnsetDetector::m_preferredStepSecs = 0.01161;

class OnsetDetectorData
{
public:
    OnsetDetectorData(const DFConfig &config) : dfConfig(config) {
	df = new DetectionFunction(config);
    }
    ~OnsetDetectorData() {
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
    

OnsetDetector::OnsetDetector(float inputSampleRate) :
    Vamp::Plugin(inputSampleRate),
    m_d(0),
    m_dfType(DF_COMPLEXSD),
    m_sensitivity(50),
    m_whiten(false)
{
}

OnsetDetector::~OnsetDetector()
{
    delete m_d;
}

string
OnsetDetector::getIdentifier() const
{
    return "qm-onsetdetector";
}

string
OnsetDetector::getName() const
{
    return "Note Onset Detector";
}

string
OnsetDetector::getDescription() const
{
    return "Estimate individual note onset positions";
}

string
OnsetDetector::getMaker() const
{
    return "Queen Mary, University of London";
}

int
OnsetDetector::getPluginVersion() const
{
    return 3;
}

string
OnsetDetector::getCopyright() const
{
    return "Plugin by Christian Landone, Chris Duxbury and Juan Pablo Bello.  Copyright (c) 2006-2009 QMUL - All Rights Reserved";
}

OnsetDetector::ParameterList
OnsetDetector::getParameterDescriptors() const
{
    ParameterList list;

    ParameterDescriptor desc;
    desc.identifier = "dftype";
    desc.name = "Onset Detection Function Type";
    desc.description = "Method used to calculate the onset detection function";
    desc.minValue = 0;
    desc.maxValue = 4;
    desc.defaultValue = 3;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    desc.valueNames.push_back("High-Frequency Content");
    desc.valueNames.push_back("Spectral Difference");
    desc.valueNames.push_back("Phase Deviation");
    desc.valueNames.push_back("Complex Domain");
    desc.valueNames.push_back("Broadband Energy Rise");
    list.push_back(desc);

    desc.identifier = "sensitivity";
    desc.name = "Onset Detector Sensitivity";
    desc.description = "Sensitivity of peak-picker for onset detection";
    desc.minValue = 0;
    desc.maxValue = 100;
    desc.defaultValue = 50;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    desc.unit = "%";
    desc.valueNames.clear();
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
    list.push_back(desc);

    return list;
}

float
OnsetDetector::getParameter(std::string name) const
{
    if (name == "dftype") {
        switch (m_dfType) {
        case DF_HFC: return 0;
        case DF_SPECDIFF: return 1;
        case DF_PHASEDEV: return 2;
        default: case DF_COMPLEXSD: return 3;
        case DF_BROADBAND: return 4;
        }
    } else if (name == "sensitivity") {
        return m_sensitivity;
    } else if (name == "whiten") {
        return m_whiten ? 1.0 : 0.0; 
    }
    return 0.0;
}

void
OnsetDetector::setParameter(std::string name, float value)
{
    if (name == "dftype") {
        int dfType = m_dfType;
        switch (lrintf(value)) {
        case 0: dfType = DF_HFC; break;
        case 1: dfType = DF_SPECDIFF; break;
        case 2: dfType = DF_PHASEDEV; break;
        default: case 3: dfType = DF_COMPLEXSD; break;
        case 4: dfType = DF_BROADBAND; break;
        }
        if (dfType == m_dfType) return;
        m_dfType = dfType;
        m_program = "";
    } else if (name == "sensitivity") {
        if (m_sensitivity == value) return;
        m_sensitivity = value;
        m_program = "";
    } else if (name == "whiten") {
        if (m_whiten == (value > 0.5)) return;
        m_whiten = (value > 0.5);
        m_program = "";
    }
}

OnsetDetector::ProgramList
OnsetDetector::getPrograms() const
{
    ProgramList programs;
    programs.push_back("");
    programs.push_back("General purpose");
    programs.push_back("Soft onsets");
    programs.push_back("Percussive onsets");
    return programs;
}

std::string
OnsetDetector::getCurrentProgram() const
{
    if (m_program == "") return "";
    else return m_program;
}

void
OnsetDetector::selectProgram(std::string program)
{
    if (program == "General purpose") {
        setParameter("dftype", 3); // complex
        setParameter("sensitivity", 50);
        setParameter("whiten", 0);
    } else if (program == "Soft onsets") {
        setParameter("dftype", 3); // complex
        setParameter("sensitivity", 40);
        setParameter("whiten", 1);
    } else if (program == "Percussive onsets") {
        setParameter("dftype", 4); // broadband energy rise
        setParameter("sensitivity", 40);
        setParameter("whiten", 0);
    } else {
        return;
    }
    m_program = program;
}

bool
OnsetDetector::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (m_d) {
	delete m_d;
	m_d = 0;
    }

    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) {
        std::cerr << "OnsetDetector::initialise: Unsupported channel count: "
                  << channels << std::endl;
        return false;
    }

    if (stepSize != getPreferredStepSize()) {
        std::cerr << "WARNING: OnsetDetector::initialise: Possibly sub-optimal step size for this sample rate: "
                  << stepSize << " (wanted " << (getPreferredStepSize()) << ")" << std::endl;
    }

    if (blockSize != getPreferredBlockSize()) {
        std::cerr << "WARNING: OnsetDetector::initialise: Possibly sub-optimal block size for this sample rate: "
                  << blockSize << " (wanted " << (getPreferredBlockSize()) << ")" << std::endl;
    }

    DFConfig dfConfig;
    dfConfig.DFType = m_dfType;
    dfConfig.stepSize = stepSize;
    dfConfig.frameLength = blockSize;
    dfConfig.dbRise = 6.0 - m_sensitivity / 16.6667;
    dfConfig.adaptiveWhitening = m_whiten;
    dfConfig.whiteningRelaxCoeff = -1;
    dfConfig.whiteningFloor = -1;
    
    m_d = new OnsetDetectorData(dfConfig);
    return true;
}

void
OnsetDetector::reset()
{
    if (m_d) m_d->reset();
}

size_t
OnsetDetector::getPreferredStepSize() const
{
    size_t step = size_t(m_inputSampleRate * m_preferredStepSecs + 0.0001);
    if (step < 1) step = 1;
//    std::cerr << "OnsetDetector::getPreferredStepSize: input sample rate is " << m_inputSampleRate << ", step size is " << step << std::endl;
    return step;
}

size_t
OnsetDetector::getPreferredBlockSize() const
{
    return getPreferredStepSize() * 2;
}

OnsetDetector::OutputList
OnsetDetector::getOutputDescriptors() const
{
    OutputList list;

    float stepSecs = m_preferredStepSecs;
//    if (m_d) stepSecs = m_d->dfConfig.stepSecs;

    OutputDescriptor onsets;
    onsets.identifier = "onsets";
    onsets.name = "Note Onsets";
    onsets.description = "Perceived note onset positions";
    onsets.unit = "";
    onsets.hasFixedBinCount = true;
    onsets.binCount = 0;
    onsets.sampleType = OutputDescriptor::VariableSampleRate;
    onsets.sampleRate = 1.0 / stepSecs;

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

    OutputDescriptor sdf;
    sdf.identifier = "smoothed_df";
    sdf.name = "Smoothed Detection Function";
    sdf.description = "Smoothed probability function used for peak-picking";
    sdf.unit = "";
    sdf.hasFixedBinCount = true;
    sdf.binCount = 1;
    sdf.hasKnownExtents = false;
    sdf.isQuantized = false;

    sdf.sampleType = OutputDescriptor::VariableSampleRate;

//!!! SV doesn't seem to handle these correctly in getRemainingFeatures
//    sdf.sampleType = OutputDescriptor::FixedSampleRate;
    sdf.sampleRate = 1.0 / stepSecs;

    list.push_back(onsets);
    list.push_back(df);
    list.push_back(sdf);

    return list;
}

OnsetDetector::FeatureSet
OnsetDetector::process(const float *const *inputBuffers,
                       Vamp::RealTime timestamp)
{
    if (!m_d) {
	cerr << "ERROR: OnsetDetector::process: "
	     << "OnsetDetector has not been initialised"
	     << endl;
	return FeatureSet();
    }

    size_t len = m_d->dfConfig.frameLength / 2 + 1;

//    float mean = 0.f;
//    for (size_t i = 0; i < len; ++i) {
////        std::cerr << inputBuffers[0][i] << " ";
//        mean += inputBuffers[0][i];
//    }
////    std::cerr << std::endl;
//    mean /= len;

//    std::cerr << "OnsetDetector::process(" << timestamp << "): "
//              << "dftype " << m_dfType << ", sens " << m_sensitivity
//              << ", len " << len << ", mean " << mean << std::endl;

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

//    std::cerr << "df: " << output << std::endl;

    returnFeatures[1].push_back(feature); // detection function is output 1
    return returnFeatures;
}

OnsetDetector::FeatureSet
OnsetDetector::getRemainingFeatures()
{
    if (!m_d) {
	cerr << "ERROR: OnsetDetector::getRemainingFeatures: "
	     << "OnsetDetector has not been initialised"
	     << endl;
	return FeatureSet();
    }

    if (m_dfType == DF_BROADBAND) {
        for (size_t i = 0; i < m_d->dfOutput.size(); ++i) {
            if (m_d->dfOutput[i] < ((110 - m_sensitivity) *
                                    m_d->dfConfig.frameLength) / 200) {
                m_d->dfOutput[i] = 0;
            }
        }
    }

    double aCoeffs[] = { 1.0000, -0.5949, 0.2348 };
    double bCoeffs[] = { 0.1600,  0.3200, 0.1600 };

    FeatureSet returnFeatures;

    PPickParams ppParams;
    ppParams.length = m_d->dfOutput.size();
    // tau and cutoff appear to be unused in PeakPicking, but I've
    // inserted some moderately plausible values rather than leave
    // them unset.  The QuadThresh values come from trial and error.
    // The rest of these are copied from ttParams in the BeatTracker
    // code: I don't claim to know whether they're good or not --cc
    ppParams.tau = m_d->dfConfig.stepSize / m_inputSampleRate;
    ppParams.alpha = 9;
    ppParams.cutoff = m_inputSampleRate/4;
    ppParams.LPOrd = 2;
    ppParams.LPACoeffs = aCoeffs;
    ppParams.LPBCoeffs = bCoeffs;
    ppParams.WinT.post = 8;
    ppParams.WinT.pre = 7;
    ppParams.QuadThresh.a = (100 - m_sensitivity) / 1000.0;
    ppParams.QuadThresh.b = 0;
    ppParams.QuadThresh.c = (100 - m_sensitivity) / 1500.0;

    PeakPicking peakPicker(ppParams);

    double *ppSrc = new double[ppParams.length];
    for (int i = 0; i < ppParams.length; ++i) {
        ppSrc[i] = m_d->dfOutput[i];
    }

    vector<int> onsets;
    peakPicker.process(ppSrc, ppParams.length, onsets);

    for (size_t i = 0; i < onsets.size(); ++i) {

        size_t index = onsets[i];

        if (m_dfType != DF_BROADBAND) {
            double prevDiff = 0.0;
            while (index > 1) {
                double diff = ppSrc[index] - ppSrc[index-1];
                if (diff < prevDiff * 0.9) break;
                prevDiff = diff;
                --index;
            }
        }

	size_t frame = index * m_d->dfConfig.stepSize;

	Feature feature;
	feature.hasTimestamp = true;
	feature.timestamp = m_d->origin + Vamp::RealTime::frame2RealTime
	    (frame, lrintf(m_inputSampleRate));

	returnFeatures[0].push_back(feature); // onsets are output 0
    }

    for (int i = 0; i < ppParams.length; ++i) {
        
        Feature feature;

        feature.hasTimestamp = true;
	size_t frame = i * m_d->dfConfig.stepSize;
	feature.timestamp = m_d->origin + Vamp::RealTime::frame2RealTime
	    (frame, lrintf(m_inputSampleRate));

        feature.values.push_back(ppSrc[i]);
        returnFeatures[2].push_back(feature); // smoothed df is output 2
    }

    return returnFeatures;
}

