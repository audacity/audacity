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

#include "MFCCPlugin.h"

#include <dsp/mfcc/MFCC.h>
#include <maths/MathUtilities.h>

using std::string;
using std::vector;
using std::cerr;
using std::endl;

MFCCPlugin::MFCCPlugin(float inputSampleRate) :
    Vamp::Plugin(inputSampleRate),
    m_config(lrintf(inputSampleRate)),
    m_mfcc(0),
    m_step(1024),
    m_block(2048),
    m_count(0)
{
    m_bins = 20;
    m_wantC0 = true;
    m_logpower = 1;

    setupConfig();
}

void
MFCCPlugin::setupConfig()
{
    m_config.FS = lrintf(m_inputSampleRate);
    m_config.fftsize = m_block;
    m_config.nceps = (m_wantC0 ? m_bins-1 : m_bins);
    m_config.want_c0 = m_wantC0;
    m_config.logpower = m_logpower;
}

MFCCPlugin::~MFCCPlugin()
{
    delete m_mfcc;
}

string
MFCCPlugin::getIdentifier() const
{
    return "qm-mfcc";
}

string
MFCCPlugin::getName() const
{
    return "Mel-Frequency Cepstral Coefficients";
}

string
MFCCPlugin::getDescription() const
{
    return "Calculate a series of MFCC vectors from the audio";
}

string
MFCCPlugin::getMaker() const
{
    return "Queen Mary, University of London";
}

int
MFCCPlugin::getPluginVersion() const
{
    return 1;
}

string
MFCCPlugin::getCopyright() const
{
    return "Plugin by Nicolas Chetry and Chris Cannam.  Copyright (c) 2009 QMUL - All Rights Reserved";
}

MFCCPlugin::ParameterList
MFCCPlugin::getParameterDescriptors() const
{
    ParameterList list;

    ParameterDescriptor desc;
    desc.identifier = "nceps";
    desc.name = "Number of Coefficients";
    desc.unit = "";
    desc.description = "Number of MFCCs to return, starting from C0 if \"Include C0\" is specified or from C1 otherwise";
    desc.minValue = 1;
    desc.maxValue = 40;
    desc.defaultValue = 20;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);

    desc.identifier = "logpower";
    desc.name = "Power for Mel Amplitude Logs";
    desc.unit = "";
    desc.description = "Power to raise the amplitude log values to before applying DCT.  Values greater than 1 may reduce contribution of noise";
    desc.minValue = 0;
    desc.maxValue = 5;
    desc.defaultValue = 1;
    desc.isQuantized = false;
    desc.quantizeStep = 0;
    list.push_back(desc);

    desc.identifier = "wantc0";
    desc.name = "Include C0";
    desc.unit = "";
    desc.description = "Whether to include the C0 (energy level) coefficient in the returned results";
    desc.minValue = 0;
    desc.maxValue = 1;
    desc.defaultValue = 1;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);

    return list;
}

float
MFCCPlugin::getParameter(std::string param) const
{
    if (param == "nceps") {
        return m_bins;
    }
    if (param == "logpower") {
        return m_logpower;
    }
    if (param == "wantc0") {
        return m_wantC0 ? 1 : 0;
    }
    std::cerr << "WARNING: MFCCPlugin::getParameter: unknown parameter \""
              << param << "\"" << std::endl;
    return 0.0;
}

void
MFCCPlugin::setParameter(std::string param, float value)
{
    if (param == "nceps") {
        m_bins = lrintf(value);
    } else if (param == "logpower") {
        m_logpower = lrintf(value);
    } else if (param == "wantc0") {
        m_wantC0 = (value > 0.5);
    } else {
        std::cerr << "WARNING: MFCCPlugin::setParameter: unknown parameter \""
                  << param << "\"" << std::endl;
    }

    setupConfig();
}

bool
MFCCPlugin::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (m_mfcc) {
	delete m_mfcc;
	m_mfcc = 0;
    }

    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) return false;

//    std::cerr << "MFCCPlugin::initialise: step " << stepSize << ", block "
//	      << blockSize << std::endl;

    m_step = stepSize;
    m_block = blockSize;
    setupConfig();

    m_mfcc = new MFCC(m_config);

    m_binsums = vector<double>(m_bins);
    for (int i = 0; i < m_bins; ++i) {
        m_binsums[i] = 0.0;
    }

    return true;
}

void
MFCCPlugin::reset()
{
    if (m_mfcc) {
	delete m_mfcc;
	m_mfcc = new MFCC(m_config);
        for (int i = 0; i < m_bins; ++i) {
            m_binsums[i] = 0.0;
        }
    }
    m_count = 0;
}

size_t
MFCCPlugin::getPreferredStepSize() const
{
    return 1024;
}

size_t
MFCCPlugin::getPreferredBlockSize() const
{
    return 2048;
}

MFCCPlugin::OutputList
MFCCPlugin::getOutputDescriptors() const
{
    OutputList list;

    OutputDescriptor d;
    d.identifier = "coefficients";
    d.name = "Coefficients";
    d.unit = "";
    d.description = "MFCC values";
    d.hasFixedBinCount = true;
    d.binCount = m_bins;
    d.hasKnownExtents = false;
    d.isQuantized = false;
    d.sampleType = OutputDescriptor::OneSamplePerStep;
    list.push_back(d);

    d.identifier = "means";
    d.name = "Means of Coefficients";
    d.description = "Mean values of MFCCs across duration of audio input";
    d.sampleType = OutputDescriptor::FixedSampleRate;
    d.sampleRate = 1;
    list.push_back(d);

    return list;
}

MFCCPlugin::FeatureSet
MFCCPlugin::process(const float *const *inputBuffers,
                    Vamp::RealTime /* timestamp */)
{
    if (!m_mfcc) {
	cerr << "ERROR: MFCCPlugin::process: "
	     << "MFCC has not been initialised"
	     << endl;
	return FeatureSet();
    }

    double *real = new double[m_block];
    double *imag = new double[m_block];

    for (size_t i = 0; i <= m_block/2; ++i) {
	real[i] = inputBuffers[0][i*2];
	if (i > 0) real[m_block - i] = real[i];
        imag[i] = inputBuffers[0][i*2+1];
        if (i > 0) imag[m_block - i] = imag[i];
    }

    double *output = new double[m_bins];

    m_mfcc->process(real, imag, output);

    delete[] real;
    delete[] imag;

    Feature feature;
    feature.hasTimestamp = false;
    for (int i = 0; i < m_bins; ++i) {
        double value = output[i];
        if (ISNAN(value)) value = 0.0;
        m_binsums[i] += value;
	feature.values.push_back(value);
    }
    feature.label = "";
    ++m_count;

    delete[] output;

    FeatureSet returnFeatures;
    returnFeatures[0].push_back(feature);
    return returnFeatures;
}

MFCCPlugin::FeatureSet
MFCCPlugin::getRemainingFeatures()
{
    Feature feature;
    feature.hasTimestamp = true;
    feature.timestamp = Vamp::RealTime::zeroTime;
  
    for (int i = 0; i < m_bins; ++i) {
        double v = m_binsums[i];
        if (m_count > 0) v /= m_count;
        feature.values.push_back(v);
    }
    feature.label = "Coefficient means";

    FeatureSet returnFeatures;
    returnFeatures[1].push_back(feature);
    return returnFeatures;
}

