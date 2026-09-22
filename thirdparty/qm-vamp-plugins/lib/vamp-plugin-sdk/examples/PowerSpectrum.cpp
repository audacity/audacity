/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2008 QMUL.
  
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

#include "PowerSpectrum.h"

using std::string;
using std::cerr;
using std::endl;

#include <math.h>

PowerSpectrum::PowerSpectrum(float inputSampleRate) :
    Plugin(inputSampleRate),
    m_blockSize(0)
{
}

PowerSpectrum::~PowerSpectrum()
{
}

string
PowerSpectrum::getIdentifier() const
{
    return "powerspectrum";
}

string
PowerSpectrum::getName() const
{
    return "Simple Power Spectrum";
}

string
PowerSpectrum::getDescription() const
{
    return "Return the power spectrum of a signal";
}

string
PowerSpectrum::getMaker() const
{
    return "Vamp SDK Example Plugins";
}

int
PowerSpectrum::getPluginVersion() const
{
    return 1;
}

string
PowerSpectrum::getCopyright() const
{
    return "Freely redistributable (BSD license)";
}

bool
PowerSpectrum::initialise(size_t channels, size_t, size_t blockSize)
{
    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) return false;

    m_blockSize = blockSize;

    return true;
}

void
PowerSpectrum::reset()
{
}

PowerSpectrum::OutputList
PowerSpectrum::getOutputDescriptors() const
{
    OutputList list;

    OutputDescriptor d;
    d.identifier = "powerspectrum";
    d.name = "Power Spectrum";
    d.description = "Power values of the frequency spectrum bins calculated from the input signal";
    d.unit = "";
    d.hasFixedBinCount = true;
    if (m_blockSize == 0) {
        // Just so as not to return "1".  This is the bin count that
        // would result from a block size of 1024, which is a likely
        // default -- but the host should always set the block size
        // before querying the bin count for certain.
        d.binCount = 513;
    } else {
        d.binCount = m_blockSize / 2 + 1;
    }
    d.hasKnownExtents = false;
    d.isQuantized = false;
    d.sampleType = OutputDescriptor::OneSamplePerStep;
    list.push_back(d);

    return list;
}

PowerSpectrum::FeatureSet
PowerSpectrum::process(const float *const *inputBuffers, Vamp::RealTime)
{
    FeatureSet fs;

    if (m_blockSize == 0) {
	cerr << "ERROR: PowerSpectrum::process: Not initialised" << endl;
	return fs;
    }

    size_t n = m_blockSize / 2 + 1;
    const float *fbuf = inputBuffers[0];

    Feature feature;
    feature.hasTimestamp = false;
    feature.values.reserve(n); // optional

    for (size_t i = 0; i < n; ++i) {

	double real = fbuf[i * 2];
	double imag = fbuf[i * 2 + 1];

        feature.values.push_back(real * real + imag * imag);
    }

    fs[0].push_back(feature);

    return fs;
}

PowerSpectrum::FeatureSet
PowerSpectrum::getRemainingFeatures()
{
    return FeatureSet();
}

