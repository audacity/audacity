/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    This file copyright 2006 Dan Stowell.
  
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

#include "AmplitudeFollower.h"

#include <cmath>

#include <string>
#include <vector>
#include <iostream>

using std::string;
using std::vector;
using std::cerr;
using std::endl;

/**
 * An implementation of SuperCollider's amplitude-follower algorithm
 * as a simple Vamp plugin.
 */

AmplitudeFollower::AmplitudeFollower(float inputSampleRate) :
    Plugin(inputSampleRate),
    m_stepSize(0),
    m_previn(0.0f),
    m_clampcoef(0.01f),
    m_relaxcoef(0.01f)
{
}

AmplitudeFollower::~AmplitudeFollower()
{
}

string
AmplitudeFollower::getIdentifier() const
{
    return "amplitudefollower";
}

string
AmplitudeFollower::getName() const
{
    return "Amplitude Follower";
}

string
AmplitudeFollower::getDescription() const
{
    return "Track the amplitude of the audio signal";
}

string
AmplitudeFollower::getMaker() const
{
    return "Vamp SDK Example Plugins";
}

int
AmplitudeFollower::getPluginVersion() const
{
    return 1;
}

string
AmplitudeFollower::getCopyright() const
{
    return "Code copyright 2006 Dan Stowell; method from SuperCollider.  Freely redistributable (BSD license)";
}

bool
AmplitudeFollower::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) return false;

    m_stepSize = std::min(stepSize, blockSize);
	
    // Translate the coefficients 
    // from their "convenient" 60dB convergence-time values
    // to real coefficients
    m_clampcoef = m_clampcoef==0.0 ? 0.0 : exp(log(0.1)/(m_clampcoef * m_inputSampleRate));
    m_relaxcoef = m_relaxcoef==0.0 ? 0.0 : exp(log(0.1)/(m_relaxcoef * m_inputSampleRate));

    return true;
}

void
AmplitudeFollower::reset()
{
    m_previn = 0.0f;
}

AmplitudeFollower::OutputList
AmplitudeFollower::getOutputDescriptors() const
{
    OutputList list;

    OutputDescriptor sca;
    sca.identifier = "amplitude";
    sca.name = "Amplitude";
    sca.description = "The peak tracked amplitude for the current processing block";
    sca.unit = "V";
    sca.hasFixedBinCount = true;
    sca.binCount = 1;
    sca.hasKnownExtents = false;
    sca.isQuantized = false;
    sca.sampleType = OutputDescriptor::OneSamplePerStep;
    list.push_back(sca);

    return list;
}

AmplitudeFollower::ParameterList
AmplitudeFollower::getParameterDescriptors() const
{
    ParameterList list;
	
    ParameterDescriptor att;
    att.identifier = "attack";
    att.name = "Attack time";
    att.description = "The 60dB convergence time for an increase in amplitude";
    att.unit = "s";
    att.minValue = 0.0f;
    att.maxValue = 1.f;
    att.defaultValue = 0.01f;
    att.isQuantized = false;
    
    list.push_back(att);
    
    ParameterDescriptor dec;
    dec.identifier = "release";
    dec.name = "Release time";
    dec.description = "The 60dB convergence time for a decrease in amplitude";
    dec.unit = "s";
    dec.minValue = 0.0f;
    dec.maxValue = 1.f;
    dec.defaultValue = 0.01f;
    dec.isQuantized = false;
    
    list.push_back(dec);
    
    return list;
}

void AmplitudeFollower::setParameter(std::string paramid, float newval)
{
    if (paramid == "attack") {
        m_clampcoef = newval;
    } else if (paramid == "release") {
        m_relaxcoef = newval;
    }
}

float AmplitudeFollower::getParameter(std::string paramid) const
{
    if (paramid == "attack") {
        return m_clampcoef;
    } else if (paramid == "release") {
        return m_relaxcoef;
    }

    return 0.0f;
}

AmplitudeFollower::FeatureSet
AmplitudeFollower::process(const float *const *inputBuffers,
                           Vamp::RealTime timestamp)
{
    if (m_stepSize == 0) {
	cerr << "ERROR: AmplitudeFollower::process: "
	     << "AmplitudeFollower has not been initialised"
	     << endl;
	return FeatureSet();
    }

    float previn = m_previn;

    FeatureSet returnFeatures;
	
    float val;
    float peak = 0.0f;

    for (size_t i = 0; i < m_stepSize; ++i) {

        val = fabs(inputBuffers[0][i]);
		
        if (val < previn) {
            val = val + (previn - val) * m_relaxcoef;
        } else {
            val = val + (previn - val) * m_clampcoef;
        }

        if (val > peak) peak = val;
        previn = val;
    }

    m_previn = previn;

    // Now store the "feature" (peak amp) for this sample
    Feature feature;
    feature.hasTimestamp = false;
    feature.values.push_back(peak);
    returnFeatures[0].push_back(feature);

    return returnFeatures;
}

AmplitudeFollower::FeatureSet
AmplitudeFollower::getRemainingFeatures()
{
    return FeatureSet();
}

