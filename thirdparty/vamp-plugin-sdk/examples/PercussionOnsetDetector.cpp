/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006 Chris Cannam.
  
    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR
    ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
    CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

    Except as contained in this notice, the names of the Centre for
    Digital Music; Queen Mary, University of London; and Chris Cannam
    shall not be used in advertising or otherwise to promote the sale,
    use or other dealings in this Software without prior written
    authorization.
*/

#include "PercussionOnsetDetector.h"

using std::string;
using std::vector;
using std::cerr;
using std::endl;

#include <cmath>


PercussionOnsetDetector::PercussionOnsetDetector(float inputSampleRate) :
    Plugin(inputSampleRate),
    m_stepSize(0),
    m_blockSize(0),
    m_threshold(3),
    m_sensitivity(40),
    m_priorMagnitudes(0),
    m_dfMinus1(0),
    m_dfMinus2(0)
{
}

PercussionOnsetDetector::~PercussionOnsetDetector()
{
    delete[] m_priorMagnitudes;
}

string
PercussionOnsetDetector::getIdentifier() const
{
    return "percussiononsets";
}

string
PercussionOnsetDetector::getName() const
{
    return "Simple Percussion Onset Detector";
}

string
PercussionOnsetDetector::getDescription() const
{
    return "Detect percussive note onsets by identifying broadband energy rises";
}

string
PercussionOnsetDetector::getMaker() const
{
    return "Vamp SDK Example Plugins";
}

int
PercussionOnsetDetector::getPluginVersion() const
{
    return 2;
}

string
PercussionOnsetDetector::getCopyright() const
{
    return "Code copyright 2006 Queen Mary, University of London, after Dan Barry et al 2005.  Freely redistributable (BSD license)";
}

size_t
PercussionOnsetDetector::getPreferredStepSize() const
{
    return 0;
}

size_t
PercussionOnsetDetector::getPreferredBlockSize() const
{
    return 1024;
}

bool
PercussionOnsetDetector::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) return false;

    m_stepSize = stepSize;
    m_blockSize = blockSize;

    m_priorMagnitudes = new float[m_blockSize/2];

    for (size_t i = 0; i < m_blockSize/2; ++i) {
        m_priorMagnitudes[i] = 0.f;
    }

    m_dfMinus1 = 0.f;
    m_dfMinus2 = 0.f;

    return true;
}

void
PercussionOnsetDetector::reset()
{
    for (size_t i = 0; i < m_blockSize/2; ++i) {
        m_priorMagnitudes[i] = 0.f;
    }

    m_dfMinus1 = 0.f;
    m_dfMinus2 = 0.f;
}

PercussionOnsetDetector::ParameterList
PercussionOnsetDetector::getParameterDescriptors() const
{
    ParameterList list;

    ParameterDescriptor d;
    d.identifier = "threshold";
    d.name = "Energy rise threshold";
    d.description = "Energy rise within a frequency bin necessary to count toward broadband total";
    d.unit = "dB";
    d.minValue = 0;
    d.maxValue = 20;
    d.defaultValue = 3;
    d.isQuantized = false;
    list.push_back(d);

    d.identifier = "sensitivity";
    d.name = "Sensitivity";
    d.description = "Sensitivity of peak detector applied to broadband detection function";
    d.unit = "%";
    d.minValue = 0;
    d.maxValue = 100;
    d.defaultValue = 40;
    d.isQuantized = false;
    list.push_back(d);

    return list;
}

float
PercussionOnsetDetector::getParameter(std::string id) const
{
    if (id == "threshold") return m_threshold;
    if (id == "sensitivity") return m_sensitivity;
    return 0.f;
}

void
PercussionOnsetDetector::setParameter(std::string id, float value)
{
    if (id == "threshold") {
        if (value < 0) value = 0;
        if (value > 20) value = 20;
        m_threshold = value;
    } else if (id == "sensitivity") {
        if (value < 0) value = 0;
        if (value > 100) value = 100;
        m_sensitivity = value;
    }
}

PercussionOnsetDetector::OutputList
PercussionOnsetDetector::getOutputDescriptors() const
{
    OutputList list;

    OutputDescriptor d;
    d.identifier = "onsets";
    d.name = "Onsets";
    d.description = "Percussive note onset locations";
    d.unit = "";
    d.hasFixedBinCount = true;
    d.binCount = 0;
    d.hasKnownExtents = false;
    d.isQuantized = false;
    d.sampleType = OutputDescriptor::VariableSampleRate;
    d.sampleRate = m_inputSampleRate;
    list.push_back(d);

    d.identifier = "detectionfunction";
    d.name = "Detection Function";
    d.description = "Broadband energy rise detection function";
    d.binCount = 1;
    d.isQuantized = true;
    d.quantizeStep = 1.0;
    d.sampleType = OutputDescriptor::OneSamplePerStep;
    list.push_back(d);

    return list;
}

PercussionOnsetDetector::FeatureSet
PercussionOnsetDetector::process(const float *const *inputBuffers,
                                 Vamp::RealTime ts)
{
    if (m_stepSize == 0) {
	cerr << "ERROR: PercussionOnsetDetector::process: "
	     << "PercussionOnsetDetector has not been initialised"
	     << endl;
	return FeatureSet();
    }

    int count = 0;

    for (size_t i = 1; i < m_blockSize/2; ++i) {

        float real = inputBuffers[0][i*2];
        float imag = inputBuffers[0][i*2 + 1];

        float sqrmag = real * real + imag * imag;

        if (m_priorMagnitudes[i] > 0.f) {
            float diff = 10.f * log10f(sqrmag / m_priorMagnitudes[i]);

//        std::cout << "i=" << i << ", sqrmag=" << sqrmag << ", prior=" << m_priorMagnitudes[i] << ", diff=" << diff << ", threshold=" << m_threshold << " " << (diff >= m_threshold ? "[*]" : "") << std::endl;

            if (diff >= m_threshold) ++count;
        }

        m_priorMagnitudes[i] = sqrmag;
    }

    FeatureSet returnFeatures;

    Feature detectionFunction;
    detectionFunction.hasTimestamp = false;
    detectionFunction.values.push_back(count);
    returnFeatures[1].push_back(detectionFunction);

    if (m_dfMinus2 < m_dfMinus1 &&
        m_dfMinus1 >= count &&
        m_dfMinus1 > ((100 - m_sensitivity) * m_blockSize) / 200) {

//std::cout << "result at " << ts << "! (count == " << count << ", prev == " << m_dfMinus1 << ")" << std::endl;

        Feature onset;
        onset.hasTimestamp = true;
        onset.timestamp = ts - Vamp::RealTime::frame2RealTime
            (m_stepSize, int(m_inputSampleRate + 0.5));
        returnFeatures[0].push_back(onset);
    }

    m_dfMinus2 = m_dfMinus1;
    m_dfMinus1 = count;

    return returnFeatures;
}

PercussionOnsetDetector::FeatureSet
PercussionOnsetDetector::getRemainingFeatures()
{
    return FeatureSet();
}

