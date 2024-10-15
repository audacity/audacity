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

#include "SpectralCentroid.h"

using std::string;
using std::vector;
using std::cerr;
using std::endl;

#include <math.h>

#ifdef __SUNPRO_CC
#include <ieeefp.h>
#define isinf(x) (!finite(x))
#endif

#ifdef WIN32
#define isnan(x) false
#define isinf(x) false
#endif

SpectralCentroid::SpectralCentroid(float inputSampleRate) :
    Plugin(inputSampleRate),
    m_stepSize(0),
    m_blockSize(0)
{
}

SpectralCentroid::~SpectralCentroid()
{
}

string
SpectralCentroid::getIdentifier() const
{
    return "spectralcentroid";
}

string
SpectralCentroid::getName() const
{
    return "Spectral Centroid";
}

string
SpectralCentroid::getDescription() const
{
    return "Calculate the centroid frequency of the spectrum of the input signal";
}

string
SpectralCentroid::getMaker() const
{
    return "Vamp SDK Example Plugins";
}

int
SpectralCentroid::getPluginVersion() const
{
    return 2;
}

string
SpectralCentroid::getCopyright() const
{
    return "Freely redistributable (BSD license)";
}

bool
SpectralCentroid::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) return false;

    m_stepSize = stepSize;
    m_blockSize = blockSize;

    return true;
}

void
SpectralCentroid::reset()
{
}

SpectralCentroid::OutputList
SpectralCentroid::getOutputDescriptors() const
{
    OutputList list;

    OutputDescriptor d;
    d.identifier = "logcentroid";
    d.name = "Log Frequency Centroid";
    d.description = "Centroid of the log weighted frequency spectrum";
    d.unit = "Hz";
    d.hasFixedBinCount = true;
    d.binCount = 1;
    d.hasKnownExtents = false;
    d.isQuantized = false;
    d.sampleType = OutputDescriptor::OneSamplePerStep;
    list.push_back(d);

    d.identifier = "linearcentroid";
    d.name = "Linear Frequency Centroid";
    d.description = "Centroid of the linear frequency spectrum";
    list.push_back(d);

    return list;
}

SpectralCentroid::FeatureSet
SpectralCentroid::process(const float *const *inputBuffers, Vamp::RealTime timestamp)
{
    if (m_stepSize == 0) {
	cerr << "ERROR: SpectralCentroid::process: "
	     << "SpectralCentroid has not been initialised"
	     << endl;
	return FeatureSet();
    }

    double numLin = 0.0, numLog = 0.0, denom = 0.0;

    for (size_t i = 1; i <= m_blockSize/2; ++i) {
	double freq = (double(i) * m_inputSampleRate) / m_blockSize;
	double real = inputBuffers[0][i*2];
	double imag = inputBuffers[0][i*2 + 1];
	double scalemag = sqrt(real * real + imag * imag) / (m_blockSize/2);
	numLin += freq * scalemag;
        numLog += log10f(freq) * scalemag;
	denom += scalemag;
    }

    FeatureSet returnFeatures;

    if (denom != 0.0) {
	float centroidLin = float(numLin / denom);
	float centroidLog = powf(10, float(numLog / denom));

	Feature feature;
	feature.hasTimestamp = false;

        if (!isnan(centroidLog) && !isinf(centroidLog)) {
            feature.values.push_back(centroidLog);
        }
	returnFeatures[0].push_back(feature);

        feature.values.clear();
        if (!isnan(centroidLin) && !isinf(centroidLin)) {
            feature.values.push_back(centroidLin);
        }
	returnFeatures[1].push_back(feature);
    }

    return returnFeatures;
}

SpectralCentroid::FeatureSet
SpectralCentroid::getRemainingFeatures()
{
    return FeatureSet();
}

