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

#include "ConstantQSpectrogram.h"

#include <base/Pitch.h>
#include <dsp/chromagram/ConstantQ.h>

using std::string;
using std::vector;
using std::cerr;
using std::endl;

ConstantQSpectrogram::ConstantQSpectrogram(float inputSampleRate) :
    Vamp::Plugin(inputSampleRate),
    m_bins(1),
    m_cq(0),
    m_step(0),
    m_block(0)
{
    m_minMIDIPitch = 36;
    m_maxMIDIPitch = 84;
    m_tuningFrequency = 440;
    m_normalized = false;
    m_bpo = 12;

    setupConfig();
}

void
ConstantQSpectrogram::setupConfig()
{
    m_config.FS = lrintf(m_inputSampleRate);
    m_config.min = Pitch::getFrequencyForPitch
        (m_minMIDIPitch, 0, m_tuningFrequency);
    m_config.max = Pitch::getFrequencyForPitch
        (m_maxMIDIPitch, 0, m_tuningFrequency);
    m_config.BPO = m_bpo;
    m_config.CQThresh = 0.0054;

    m_step = 0;
    m_block = 0;
}

ConstantQSpectrogram::~ConstantQSpectrogram()
{
    delete m_cq;
}

string
ConstantQSpectrogram::getIdentifier() const
{
    return "qm-constantq";
}

string
ConstantQSpectrogram::getName() const
{
    return "Constant-Q Spectrogram";
}

string
ConstantQSpectrogram::getDescription() const
{
    return "Extract a spectrogram with constant ratio of centre frequency to resolution from the input audio";
}

string
ConstantQSpectrogram::getMaker() const
{
    return "Queen Mary, University of London";
}

int
ConstantQSpectrogram::getPluginVersion() const
{
    return 4;
}

string
ConstantQSpectrogram::getCopyright() const
{
    return "Plugin by Chris Cannam and Christian Landone.  Copyright (c) 2006-2009 QMUL - All Rights Reserved";
}

ConstantQSpectrogram::ParameterList
ConstantQSpectrogram::getParameterDescriptors() const
{
    ParameterList list;

    ParameterDescriptor desc;
    desc.identifier = "minpitch";
    desc.name = "Minimum Pitch";
    desc.unit = "MIDI units";
    desc.description = "MIDI pitch corresponding to the lowest frequency to be included in the constant-Q transform";
    desc.minValue = 0;
    desc.maxValue = 127;
    desc.defaultValue = 36;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);

    desc.identifier = "maxpitch";
    desc.name = "Maximum Pitch";
    desc.unit = "MIDI units";
    desc.description = "MIDI pitch corresponding to the highest frequency to be included in the constant-Q transform";
    desc.minValue = 0;
    desc.maxValue = 127;
    desc.defaultValue = 84;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);

    desc.identifier = "tuning";
    desc.name = "Tuning Frequency";
    desc.unit = "Hz";
    desc.description = "Frequency of concert A";
    desc.minValue = 360;
    desc.maxValue = 500;
    desc.defaultValue = 440;
    desc.isQuantized = false;
    list.push_back(desc);
    
    desc.identifier = "bpo";
    desc.name = "Bins per Octave";
    desc.unit = "bins";
    desc.description = "Number of constant-Q transform bins per octave";
    desc.minValue = 2;
    desc.maxValue = 480;
    desc.defaultValue = 12;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);

    desc.identifier = "normalized";
    desc.name = "Normalized";
    desc.unit = "";
    desc.description = "Whether to normalize each output column to unit maximum";
    desc.minValue = 0;
    desc.maxValue = 1;
    desc.defaultValue = 0;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);

    return list;
}

float
ConstantQSpectrogram::getParameter(std::string param) const
{
    if (param == "minpitch") {
        return m_minMIDIPitch;
    }
    if (param == "maxpitch") {
        return m_maxMIDIPitch;
    }
    if (param == "tuning") {
        return m_tuningFrequency;
    }
    if (param == "bpo") {
        return m_bpo;
    }
    if (param == "normalized") {
        return m_normalized;
    }
    std::cerr << "WARNING: ConstantQSpectrogram::getParameter: unknown parameter \""
              << param << "\"" << std::endl;
    return 0.0;
}

void
ConstantQSpectrogram::setParameter(std::string param, float value)
{
    if (param == "minpitch") {
        m_minMIDIPitch = lrintf(value);
    } else if (param == "maxpitch") {
        m_maxMIDIPitch = lrintf(value);
    } else if (param == "tuning") {
        m_tuningFrequency = value;
    } else if (param == "bpo") {
        m_bpo = lrintf(value);
    } else if (param == "normalized") {
        m_normalized = (value > 0.0001);
    } else {
        std::cerr << "WARNING: ConstantQSpectrogram::setParameter: unknown parameter \""
                  << param << "\"" << std::endl;
    }

    setupConfig();
}


bool
ConstantQSpectrogram::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (m_cq) {
	delete m_cq;
	m_cq = 0;
    }

    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) return false;

    if (m_inputSampleRate > 384000) {
        std::cerr << "ConstantQSpectrogram::initialise: Maximum input sample rate is 384000" << std::endl;
        return false;
    }

    setupConfig();

    m_cq = new ConstantQ(m_config);
    m_bins = m_cq->getK();
    m_cq->sparsekernel();
    m_step = m_cq->getHop();
    m_block = m_cq->getFFTLength();

    if (blockSize != m_block) {
        std::cerr << "ConstantQSpectrogram::initialise: ERROR: supplied block size " << blockSize << " differs from required block size " << m_block << ", initialise failing" << std::endl;
        delete m_cq;
        m_cq = 0;
        return false;
    }

    if (stepSize != m_step) {
        std::cerr << "ConstantQSpectrogram::initialise: NOTE: supplied step size " << stepSize << " differs from expected step size " << m_step << " (for block size = " << m_block << ")" << std::endl;
    }

    return true;
}

void
ConstantQSpectrogram::reset()
{
    if (m_cq) {
	delete m_cq;
	m_cq = new ConstantQ(m_config);
        m_bins = m_cq->getK();
        m_cq->sparsekernel();
        m_step = m_cq->getHop();
        m_block = m_cq->getFFTLength();
    }
}

size_t
ConstantQSpectrogram::getPreferredStepSize() const
{
    if (!m_step) {
	ConstantQ cq(m_config);
	m_step = cq.getHop();
	m_block = cq.getFFTLength();
    }

    return m_step;
}

size_t
ConstantQSpectrogram::getPreferredBlockSize() const
{
    if (!m_block) {
	ConstantQ cq(m_config);
	m_step = cq.getHop();
	m_block = cq.getFFTLength();
    }

    return m_block;
}

ConstantQSpectrogram::OutputList
ConstantQSpectrogram::getOutputDescriptors() const
{
    OutputList list;

    OutputDescriptor d;
    d.identifier = "constantq";
    d.name = "Constant-Q Spectrogram";
    d.unit = "";
    d.description = "Output of constant-Q transform, as a single vector per process block";
    d.hasFixedBinCount = true;
    d.binCount = m_bins;

//    std::cerr << "Bin count " << d.binCount << std::endl;
    
    const char *names[] =
	{ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" };

    if (m_bpo == 12) {
        for (int i = 0; i < int(d.binCount); ++i) {
            int ipc = m_minMIDIPitch % 12;
            int index = (i + ipc) % 12;
            d.binNames.push_back(names[index]);
        }
    } else {
        d.binNames.push_back(names[m_minMIDIPitch % 12]);
    }

    d.hasKnownExtents = m_normalized;
    d.minValue = 0.0;
    d.maxValue = (m_normalized ? 1.0 : 0.0);
    d.isQuantized = false;
    d.sampleType = OutputDescriptor::OneSamplePerStep;
    list.push_back(d);

    return list;
}

ConstantQSpectrogram::Feature
ConstantQSpectrogram::normalize(const Feature &feature)
{
    float min = 0.0, max = 0.0;

    for (size_t i = 0; i < feature.values.size(); ++i) {
	if (i == 0 || feature.values[i] < min) min = feature.values[i];
	if (i == 0 || feature.values[i] > max) max = feature.values[i];
    }
	
    if (max == 0.0 || max == min) return feature;

    Feature normalized;
    normalized.hasTimestamp = false;

    for (size_t i = 0; i < feature.values.size(); ++i) {
	normalized.values.push_back((feature.values[i] - min) / (max - min));
    }

    return normalized;
}

ConstantQSpectrogram::FeatureSet
ConstantQSpectrogram::process(const float *const *inputBuffers,
                              Vamp::RealTime /* timestamp */)
{
    if (!m_cq) {
	cerr << "ERROR: ConstantQSpectrogram::process: "
	     << "Constant-Q has not been initialised"
	     << endl;
	return FeatureSet();
    }

    double *real = new double[m_block];
    double *imag = new double[m_block];
    double *cqre = new double[m_bins];
    double *cqim = new double[m_bins];

//    std::cout << "in:" << std::endl;
    for (size_t i = 0; i <= m_block/2; ++i) {
	real[i] = inputBuffers[0][i*2];
	if (i > 0) real[m_block - i] = real[i];
        imag[i] = inputBuffers[0][i*2+1];
        if (i > 0) imag[m_block - i] = imag[i]; //!!! huh? surely -imag[i] ?
//        std::cout << real[i] << "," << imag[i] << " ";
    }

    m_cq->process(real, imag, cqre, cqim);

    delete[] real;
    delete[] imag;

//    std::cout << "\nout:" << std::endl;
    Feature feature;
    feature.hasTimestamp = false;
    for (int i = 0; i < m_bins; ++i) {
        double re = cqre[i];
        double im = cqim[i];
//        std::cout << re << "," << im << ":";
        if (ISNAN(re)) re = 0.0;
        if (ISNAN(im)) im = 0.0;
        double value = sqrt(re * re + im * im);
//        std::cout << value << " ";
	feature.values.push_back(value);
    }
    feature.label = "";

    delete[] cqre;
    delete[] cqim;

    FeatureSet returnFeatures;
    if (m_normalized) returnFeatures[0].push_back(normalize(feature));
    else returnFeatures[0].push_back(feature);
    return returnFeatures;
}

ConstantQSpectrogram::FeatureSet
ConstantQSpectrogram::getRemainingFeatures()
{
    return FeatureSet();
}

