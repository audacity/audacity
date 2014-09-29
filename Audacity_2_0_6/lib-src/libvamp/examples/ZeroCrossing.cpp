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

#include "ZeroCrossing.h"

using std::string;
using std::vector;
using std::cerr;
using std::endl;

#include <cmath>

ZeroCrossing::ZeroCrossing(float inputSampleRate) :
    Plugin(inputSampleRate),
    m_stepSize(0),
    m_previousSample(0.0f)
{
}

ZeroCrossing::~ZeroCrossing()
{
}

string
ZeroCrossing::getIdentifier() const
{
    return "zerocrossing";
}

string
ZeroCrossing::getName() const
{
    return "Zero Crossings";
}

string
ZeroCrossing::getDescription() const
{
    return "Detect and count zero crossing points";
}

string
ZeroCrossing::getMaker() const
{
    return "Vamp SDK Example Plugins";
}

int
ZeroCrossing::getPluginVersion() const
{
    return 2;
}

string
ZeroCrossing::getCopyright() const
{
    return "Freely redistributable (BSD license)";
}

bool
ZeroCrossing::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) return false;

    m_stepSize = std::min(stepSize, blockSize);

    return true;
}

void
ZeroCrossing::reset()
{
    m_previousSample = 0.0f;
}

ZeroCrossing::OutputList
ZeroCrossing::getOutputDescriptors() const
{
    OutputList list;

    OutputDescriptor zc;
    zc.identifier = "counts";
    zc.name = "Zero Crossing Counts";
    zc.description = "The number of zero crossing points per processing block";
    zc.unit = "crossings";
    zc.hasFixedBinCount = true;
    zc.binCount = 1;
    zc.hasKnownExtents = false;
    zc.isQuantized = true;
    zc.quantizeStep = 1.0;
    zc.sampleType = OutputDescriptor::OneSamplePerStep;
    list.push_back(zc);

    zc.identifier = "zerocrossings";
    zc.name = "Zero Crossings";
    zc.description = "The locations of zero crossing points";
    zc.unit = "";
    zc.hasFixedBinCount = true;
    zc.binCount = 0;
    zc.sampleType = OutputDescriptor::VariableSampleRate;
    zc.sampleRate = m_inputSampleRate;
    list.push_back(zc);

    return list;
}

ZeroCrossing::FeatureSet
ZeroCrossing::process(const float *const *inputBuffers,
                      Vamp::RealTime timestamp)
{
    if (m_stepSize == 0) {
	cerr << "ERROR: ZeroCrossing::process: "
	     << "ZeroCrossing has not been initialised"
	     << endl;
	return FeatureSet();
    }

    float prev = m_previousSample;
    size_t count = 0;

    FeatureSet returnFeatures;

    for (size_t i = 0; i < m_stepSize; ++i) {

	float sample = inputBuffers[0][i];
	bool crossing = false;

	if (sample <= 0.0) {
	    if (prev > 0.0) crossing = true;
	} else if (sample > 0.0) {
	    if (prev <= 0.0) crossing = true;
	}

	if (crossing) {
	    ++count; 
	    Feature feature;
	    feature.hasTimestamp = true;
	    feature.timestamp = timestamp +
		Vamp::RealTime::frame2RealTime(i, (size_t)m_inputSampleRate);
	    returnFeatures[1].push_back(feature);
	}

	prev = sample;
    }

    m_previousSample = prev;

    Feature feature;
    feature.hasTimestamp = false;
    feature.values.push_back(float(count));

    returnFeatures[0].push_back(feature);
    return returnFeatures;
}

ZeroCrossing::FeatureSet
ZeroCrossing::getRemainingFeatures()
{
    return FeatureSet();
}

