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

#include "ChromagramPlugin.h"

#include <base/Pitch.h>
#include <dsp/chromagram/Chromagram.h>

using std::string;
using std::vector;
using std::cerr;
using std::endl;

ChromagramPlugin::ChromagramPlugin(float inputSampleRate) :
    Vamp::Plugin(inputSampleRate),
    m_chromagram(0),
    m_step(0),
    m_block(0)
{
    m_minMIDIPitch = 36;
    m_maxMIDIPitch = 96;
    m_tuningFrequency = 440;
    m_normalise = MathUtilities::NormaliseNone;
    m_bpo = 12;

    setupConfig();
}

void
ChromagramPlugin::setupConfig()
{
    m_config.FS = lrintf(m_inputSampleRate);
    m_config.min = Pitch::getFrequencyForPitch
        (m_minMIDIPitch, 0, m_tuningFrequency);
    m_config.max = Pitch::getFrequencyForPitch
        (m_maxMIDIPitch, 0, m_tuningFrequency);
    m_config.BPO = m_bpo;
    m_config.CQThresh = 0.0054;
    m_config.normalise = m_normalise;

    m_step = 0;
    m_block = 0;
}

ChromagramPlugin::~ChromagramPlugin()
{
    delete m_chromagram;
}

string
ChromagramPlugin::getIdentifier() const
{
    return "qm-chromagram";
}

string
ChromagramPlugin::getName() const
{
    return "Chromagram";
}

string
ChromagramPlugin::getDescription() const
{
    return "Extract a series of tonal chroma vectors from the audio";
}

string
ChromagramPlugin::getMaker() const
{
    return "Queen Mary, University of London";
}

int
ChromagramPlugin::getPluginVersion() const
{
    return 5;
}

string
ChromagramPlugin::getCopyright() const
{
    return "Plugin by Chris Cannam and Christian Landone.  Copyright (c) 2006-2009 QMUL - All Rights Reserved";
}

ChromagramPlugin::ParameterList
ChromagramPlugin::getParameterDescriptors() const
{
    ParameterList list;

    ParameterDescriptor desc;
    desc.identifier = "minpitch";
    desc.name = "Minimum Pitch";
    desc.unit = "MIDI units";
    desc.description = "MIDI pitch corresponding to the lowest frequency to be included in the chromagram";
    desc.minValue = 0;
    desc.maxValue = 127;
    desc.defaultValue = 36;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);

    desc.identifier = "maxpitch";
    desc.name = "Maximum Pitch";
    desc.unit = "MIDI units";
    desc.description = "MIDI pitch corresponding to the highest frequency to be included in the chromagram";
    desc.minValue = 0;
    desc.maxValue = 127;
    desc.defaultValue = 96;
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
    desc.description = "Number of constant-Q transform bins per octave, and the number of bins for the chromagram outputs";
    desc.minValue = 2;
    desc.maxValue = 480;
    desc.defaultValue = 12;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);

    desc.identifier = "normalization";
    desc.name = "Normalization";
    desc.unit = "";
    desc.description = "Normalization for each chromagram output column";
    desc.minValue = 0;
    desc.maxValue = 2;
    desc.defaultValue = 0;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    desc.valueNames.push_back("None");
    desc.valueNames.push_back("Unit Sum");
    desc.valueNames.push_back("Unit Maximum");
    list.push_back(desc);

    return list;
}

float
ChromagramPlugin::getParameter(std::string param) const
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
    if (param == "normalization") {
        return int(m_normalise);
    }
    std::cerr << "WARNING: ChromagramPlugin::getParameter: unknown parameter \""
              << param << "\"" << std::endl;
    return 0.0;
}

void
ChromagramPlugin::setParameter(std::string param, float value)
{
    if (param == "minpitch") {
        m_minMIDIPitch = lrintf(value);
    } else if (param == "maxpitch") {
        m_maxMIDIPitch = lrintf(value);
    } else if (param == "tuning") {
        m_tuningFrequency = value;
    } else if (param == "bpo") {
        m_bpo = lrintf(value);
    } else if (param == "normalization") {
        m_normalise = MathUtilities::NormaliseType(int(value + 0.0001));
    } else {
        std::cerr << "WARNING: ChromagramPlugin::setParameter: unknown parameter \""
                  << param << "\"" << std::endl;
    }

    setupConfig();
}


bool
ChromagramPlugin::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (m_chromagram) {
	delete m_chromagram;
	m_chromagram = 0;
    }

    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) return false;

    if (m_inputSampleRate > 384000) {
        std::cerr << "ChromagramPlugin::initialise: Maximum input sample rate is 384000" << std::endl;
        return false;
    }

    m_chromagram = new Chromagram(m_config);
    m_binsums = vector<double>(m_config.BPO);

    for (int i = 0; i < m_config.BPO; ++i) {
        m_binsums[i] = 0.0;
    }

    m_count = 0;

    m_step = m_chromagram->getHopSize();
    m_block = m_chromagram->getFrameSize();
    if (m_step < 1) m_step = 1;

    if (blockSize != m_block) {
        std::cerr << "ChromagramPlugin::initialise: ERROR: supplied block size " << blockSize << " differs from required block size " << m_block << ", initialise failing" << std::endl;
        delete m_chromagram;
        m_chromagram = 0;
        return false;
    }

    if (stepSize != m_step) {
        std::cerr << "ChromagramPlugin::initialise: NOTE: supplied step size " << stepSize << " differs from expected step size " << m_step << " (for block size = " << m_block << ")" << std::endl;
    }

    return true;
}

void
ChromagramPlugin::reset()
{
    if (m_chromagram) {
	delete m_chromagram;
	m_chromagram = new Chromagram(m_config);
        for (int i = 0; i < m_config.BPO; ++i) {
            m_binsums[i] = 0.0;
        }
        m_count = 0;
    }
}

size_t
ChromagramPlugin::getPreferredStepSize() const
{
    if (!m_step) {
	Chromagram chroma(m_config);
	m_step = chroma.getHopSize();
	m_block = chroma.getFrameSize();
        if (m_step < 1) m_step = 1;
    }

    return m_step;
}

size_t
ChromagramPlugin::getPreferredBlockSize() const
{
    if (!m_block) {
	Chromagram chroma(m_config);
	m_step = chroma.getHopSize();
	m_block = chroma.getFrameSize();
        if (m_step < 1) m_step = 1;
    }

    return m_block;
}

ChromagramPlugin::OutputList
ChromagramPlugin::getOutputDescriptors() const
{
    OutputList list;

    OutputDescriptor d;
    d.identifier = "chromagram";
    d.name = "Chromagram";
    d.unit = "";
    d.description = "Output of chromagram, as a single vector per process block";
    d.hasFixedBinCount = true;
    d.binCount = m_config.BPO;
    
    const char *names[] =
	{ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" };

    if (d.binCount % 12 == 0) {
        for (int i = 0; i < 12; ++i) {
            int ipc = m_minMIDIPitch % 12;
            int index = (i + ipc) % 12;
            d.binNames.push_back(names[index]);
            for (int j = 0; j < int(d.binCount) / 12 - 1; ++j) {
                d.binNames.push_back("");
            }
        }
    } else {
        d.binNames.push_back(names[m_minMIDIPitch % 12]);
    }

    d.hasKnownExtents = (m_normalise != MathUtilities::NormaliseNone);
    d.minValue = 0.0;
    d.maxValue = (d.hasKnownExtents ? 1.0 : 0.0);
    d.isQuantized = false;
    d.sampleType = OutputDescriptor::OneSamplePerStep;
    list.push_back(d);

    d.identifier = "chromameans";
    d.name = "Chroma Means";
    d.description = "Mean values of chromagram bins across the duration of the input audio";
    d.sampleType = OutputDescriptor::FixedSampleRate;
    d.sampleRate = 1;
    list.push_back(d);

    return list;
}

ChromagramPlugin::FeatureSet
ChromagramPlugin::process(const float *const *inputBuffers,
                          Vamp::RealTime )
{
    if (!m_chromagram) {
	cerr << "ERROR: ChromagramPlugin::process: "
	     << "Chromagram has not been initialised"
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

//    cerr << "chromagram: timestamp = " << timestamp << endl;
/*
    bool printThis = false;

    if (timestamp.sec == 3 && timestamp.nsec < 250000000) {
        printThis = true;
    } 
    if (printThis) {
        cerr << "\n\nchromagram: timestamp " << timestamp << ": input data starts:" << endl;
        for (int i = 0; i < m_block && i < 1000; ++i) {
            cerr << real[i] << "," << imag[i] << " ";
        }
        cerr << endl << "values:" << endl;
    }
*/
    double *output = m_chromagram->process(real, imag);

    delete[] real;
    delete[] imag;

    Feature feature;
    feature.hasTimestamp = false;
    for (int i = 0; i < m_config.BPO; ++i) {
        double value = output[i];
/*
        if (printThis) {
            cerr << value << " ";
        }
*/
        if (ISNAN(value)) value = 0.0;
        m_binsums[i] += value;
	feature.values.push_back(value);
    }
    feature.label = "";
    ++m_count;
/*
    if (printThis) {
        cerr << endl;
    }
*/

    FeatureSet returnFeatures;
    returnFeatures[0].push_back(feature);
    return returnFeatures;
}

ChromagramPlugin::FeatureSet
ChromagramPlugin::getRemainingFeatures()
{
    Feature feature;
    feature.hasTimestamp = true;
    feature.timestamp = Vamp::RealTime::zeroTime;
  
    for (int i = 0; i < m_config.BPO; ++i) {
        double v = m_binsums[i];
        if (m_count > 0) v /= m_count;
        feature.values.push_back(v);
    }
    feature.label = "Chromagram bin means";

    FeatureSet returnFeatures;
    returnFeatures[1].push_back(feature);
    return returnFeatures;
}

