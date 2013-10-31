/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006-2009 Chris Cannam and QMUL.
  
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

#include "FixedTempoEstimator.h"

using std::string;
using std::vector;
using std::cerr;
using std::endl;

using Vamp::RealTime;

#include <cmath>
#include <cstdio>


class FixedTempoEstimator::D
// this class just avoids us having to declare any data members in the header
{
public:
    D(float inputSampleRate);
    ~D();

    size_t getPreferredStepSize() const { return 64; }
    size_t getPreferredBlockSize() const { return 256; }

    ParameterList getParameterDescriptors() const;
    float getParameter(string id) const;
    void setParameter(string id, float value);

    OutputList getOutputDescriptors() const;

    bool initialise(size_t channels, size_t stepSize, size_t blockSize);
    void reset();
    FeatureSet process(const float *const *, RealTime);
    FeatureSet getRemainingFeatures();

private:
    void calculate();
    FeatureSet assembleFeatures();

    float lag2tempo(int);
    int tempo2lag(float);

    float m_inputSampleRate;
    size_t m_stepSize;
    size_t m_blockSize;

    float m_minbpm;
    float m_maxbpm;
    float m_maxdflen;

    float *m_priorMagnitudes;

    size_t m_dfsize;
    float *m_df;
    float *m_r;
    float *m_fr;
    float *m_t;
    size_t m_n;

    Vamp::RealTime m_start;
    Vamp::RealTime m_lasttime;
};

FixedTempoEstimator::D::D(float inputSampleRate) :
    m_inputSampleRate(inputSampleRate),
    m_stepSize(0),
    m_blockSize(0),
    m_minbpm(50),
    m_maxbpm(190),
    m_maxdflen(10),
    m_priorMagnitudes(0),
    m_df(0),
    m_r(0),
    m_fr(0),
    m_t(0),
    m_n(0)
{
}

FixedTempoEstimator::D::~D()
{
    delete[] m_priorMagnitudes;
    delete[] m_df;
    delete[] m_r;
    delete[] m_fr;
    delete[] m_t;
}

FixedTempoEstimator::ParameterList
FixedTempoEstimator::D::getParameterDescriptors() const
{
    ParameterList list;

    ParameterDescriptor d;
    d.identifier = "minbpm";
    d.name = "Minimum estimated tempo";
    d.description = "Minimum beat-per-minute value which the tempo estimator is able to return";
    d.unit = "bpm";
    d.minValue = 10;
    d.maxValue = 360;
    d.defaultValue = 50;
    d.isQuantized = false;
    list.push_back(d);

    d.identifier = "maxbpm";
    d.name = "Maximum estimated tempo";
    d.description = "Maximum beat-per-minute value which the tempo estimator is able to return";
    d.defaultValue = 190;
    list.push_back(d);

    d.identifier = "maxdflen";
    d.name = "Input duration to study";
    d.description = "Length of audio input, in seconds, which should be taken into account when estimating tempo.  There is no need to supply the plugin with any further input once this time has elapsed since the start of the audio.  The tempo estimator may use only the first part of this, up to eight times the slowest beat duration: increasing this value further than that is unlikely to improve results.";
    d.unit = "s";
    d.minValue = 2;
    d.maxValue = 40;
    d.defaultValue = 10;
    list.push_back(d);

    return list;
}

float
FixedTempoEstimator::D::getParameter(string id) const
{
    if (id == "minbpm") {
        return m_minbpm;
    } else if (id == "maxbpm") {
        return m_maxbpm;
    } else if (id == "maxdflen") {
        return m_maxdflen;
    }
    return 0.f;
}

void
FixedTempoEstimator::D::setParameter(string id, float value)
{
    if (id == "minbpm") {
        m_minbpm = value;
    } else if (id == "maxbpm") {
        m_maxbpm = value;
    } else if (id == "maxdflen") {
        m_maxdflen = value;
    }
}

static int TempoOutput = 0;
static int CandidatesOutput = 1;
static int DFOutput = 2;
static int ACFOutput = 3;
static int FilteredACFOutput = 4;

FixedTempoEstimator::OutputList
FixedTempoEstimator::D::getOutputDescriptors() const
{
    OutputList list;

    OutputDescriptor d;
    d.identifier = "tempo";
    d.name = "Tempo";
    d.description = "Estimated tempo";
    d.unit = "bpm";
    d.hasFixedBinCount = true;
    d.binCount = 1;
    d.hasKnownExtents = false;
    d.isQuantized = false;
    d.sampleType = OutputDescriptor::VariableSampleRate;
    d.sampleRate = m_inputSampleRate;
    d.hasDuration = true; // our returned tempo spans a certain range
    list.push_back(d);

    d.identifier = "candidates";
    d.name = "Tempo candidates";
    d.description = "Possible tempo estimates, one per bin with the most likely in the first bin";
    d.unit = "bpm";
    d.hasFixedBinCount = false;
    list.push_back(d);

    d.identifier = "detectionfunction";
    d.name = "Detection Function";
    d.description = "Onset detection function";
    d.unit = "";
    d.hasFixedBinCount = 1;
    d.binCount = 1;
    d.hasKnownExtents = true;
    d.minValue = 0.0;
    d.maxValue = 1.0;
    d.isQuantized = false;
    d.quantizeStep = 0.0;
    d.sampleType = OutputDescriptor::FixedSampleRate;
    if (m_stepSize) {
        d.sampleRate = m_inputSampleRate / m_stepSize;
    } else {
        d.sampleRate = m_inputSampleRate / (getPreferredBlockSize()/2);
    }
    d.hasDuration = false;
    list.push_back(d);

    d.identifier = "acf";
    d.name = "Autocorrelation Function";
    d.description = "Autocorrelation of onset detection function";
    d.hasKnownExtents = false;
    d.unit = "r";
    list.push_back(d);

    d.identifier = "filtered_acf";
    d.name = "Filtered Autocorrelation";
    d.description = "Filtered autocorrelation of onset detection function";
    d.unit = "r";
    list.push_back(d);

    return list;
}

bool
FixedTempoEstimator::D::initialise(size_t, size_t stepSize, size_t blockSize)
{
    m_stepSize = stepSize;
    m_blockSize = blockSize;

    float dfLengthSecs = m_maxdflen;
    m_dfsize = (dfLengthSecs * m_inputSampleRate) / m_stepSize;

    m_priorMagnitudes = new float[m_blockSize/2];
    m_df = new float[m_dfsize];

    for (size_t i = 0; i < m_blockSize/2; ++i) {
        m_priorMagnitudes[i] = 0.f;
    }
    for (size_t i = 0; i < m_dfsize; ++i) {
        m_df[i] = 0.f;
    }

    m_n = 0;

    return true;
}

void
FixedTempoEstimator::D::reset()
{
    if (!m_priorMagnitudes) return;

    for (size_t i = 0; i < m_blockSize/2; ++i) {
        m_priorMagnitudes[i] = 0.f;
    }
    for (size_t i = 0; i < m_dfsize; ++i) {
        m_df[i] = 0.f;
    }

    delete[] m_r;
    m_r = 0;

    delete[] m_fr; 
    m_fr = 0;

    delete[] m_t; 
    m_t = 0;

    m_n = 0;

    m_start = RealTime::zeroTime;
    m_lasttime = RealTime::zeroTime;
}

FixedTempoEstimator::FeatureSet
FixedTempoEstimator::D::process(const float *const *inputBuffers, RealTime ts)
{
    FeatureSet fs;

    if (m_stepSize == 0) {
	cerr << "ERROR: FixedTempoEstimator::process: "
	     << "FixedTempoEstimator has not been initialised"
	     << endl;
	return fs;
    }

    if (m_n == 0) m_start = ts;
    m_lasttime = ts;

    if (m_n == m_dfsize) {
        // If we have seen enough input, do the estimation and return
        calculate();
        fs = assembleFeatures();
        ++m_n;
        return fs;
    }

    // If we have seen more than enough, just discard and return!
    if (m_n > m_dfsize) return FeatureSet();

    float value = 0.f;

    // m_df will contain an onset detection function based on the rise
    // in overall power from one spectral frame to the next --
    // simplistic but reasonably effective for our purposes.

    for (size_t i = 1; i < m_blockSize/2; ++i) {

        float real = inputBuffers[0][i*2];
        float imag = inputBuffers[0][i*2 + 1];

        float sqrmag = real * real + imag * imag;
        value += fabsf(sqrmag - m_priorMagnitudes[i]);

        m_priorMagnitudes[i] = sqrmag;
    }

    m_df[m_n] = value;

    ++m_n;
    return fs;
}    

FixedTempoEstimator::FeatureSet
FixedTempoEstimator::D::getRemainingFeatures()
{
    FeatureSet fs;
    if (m_n > m_dfsize) return fs;
    calculate();
    fs = assembleFeatures();
    ++m_n;
    return fs;
}

float
FixedTempoEstimator::D::lag2tempo(int lag)
{
    return 60.f / ((lag * m_stepSize) / m_inputSampleRate);
}

int
FixedTempoEstimator::D::tempo2lag(float tempo)
{
    return ((60.f / tempo) * m_inputSampleRate) / m_stepSize;
}

void
FixedTempoEstimator::D::calculate()
{    
    if (m_r) {
        cerr << "FixedTempoEstimator::calculate: calculation already happened?" << endl;
        return;
    }

    if (m_n < m_dfsize / 9 &&
        m_n < (1.0 * m_inputSampleRate) / m_stepSize) { // 1 second
        cerr << "FixedTempoEstimator::calculate: Input is too short" << endl;
        return;
    }

    // This function takes m_df (the detection function array filled
    // out in process()) and calculates m_r (the raw autocorrelation)
    // and m_fr (the filtered autocorrelation from whose peaks tempo
    // estimates will be taken).

    int n = m_n; // length of actual df array (m_dfsize is the theoretical max)

    m_r  = new float[n/2]; // raw autocorrelation
    m_fr = new float[n/2]; // filtered autocorrelation
    m_t  = new float[n/2]; // averaged tempo estimate for each lag value

    for (int i = 0; i < n/2; ++i) {
        m_r[i]  = 0.f;
        m_fr[i] = 0.f;
        m_t[i]  = lag2tempo(i);
    }

    // Calculate the raw autocorrelation of the detection function

    for (int i = 0; i < n/2; ++i) {

        for (int j = i; j < n; ++j) {
            m_r[i] += m_df[j] * m_df[j - i];
        }

        m_r[i] /= n - i - 1;
    }

    // Filter the autocorrelation and average out the tempo estimates
    
    float related[] = { 0.5, 2, 4, 8 };

    for (int i = 1; i < n/2-1; ++i) {

        m_fr[i] = m_r[i];

        int div = 1;

        for (int j = 0; j < int(sizeof(related)/sizeof(related[0])); ++j) {

            // Check for an obvious peak at each metrically related lag

            int k0 = int(i * related[j] + 0.5);

            if (k0 >= 0 && k0 < int(n/2)) {

                int kmax = 0, kmin = 0;
                float kvmax = 0, kvmin = 0;
                bool have = false;

                for (int k = k0 - 1; k <= k0 + 1; ++k) {

                    if (k < 0 || k >= n/2) continue;

                    if (!have || (m_r[k] > kvmax)) { kmax = k; kvmax = m_r[k]; }
                    if (!have || (m_r[k] < kvmin)) { kmin = k; kvmin = m_r[k]; }
                    
                    have = true;
                }
                
                // Boost the original lag according to the strongest
                // value found close to this related lag

                m_fr[i] += m_r[kmax] / 5;

                if ((kmax == 0 || m_r[kmax] > m_r[kmax-1]) &&
                    (kmax == n/2-1 || m_r[kmax] > m_r[kmax+1]) &&
                    kvmax > kvmin * 1.05) {

                    // The strongest value close to the related lag is
                    // also a pretty good looking peak, so use it to
                    // improve our tempo estimate for the original lag
                    
                    m_t[i] = m_t[i] + lag2tempo(kmax) * related[j];
                    ++div;
                }
            }
        }
        
        m_t[i] /= div;
        
        // Finally apply a primitive perceptual weighting (to prefer
        // tempi of around 120-130)

        float weight = 1.f - fabsf(128.f - lag2tempo(i)) * 0.005;
        if (weight < 0.f) weight = 0.f;
        weight = weight * weight * weight;

        m_fr[i] += m_fr[i] * (weight / 3);
    }
}
    
FixedTempoEstimator::FeatureSet
FixedTempoEstimator::D::assembleFeatures()
{
    FeatureSet fs;
    if (!m_r) return fs; // No autocorrelation: no results

    Feature feature;
    feature.hasTimestamp = true;
    feature.hasDuration = false;
    feature.label = "";
    feature.values.clear();
    feature.values.push_back(0.f);

    char buffer[40];

    int n = m_n;

    for (int i = 0; i < n; ++i) {

        // Return the detection function in the DF output

        feature.timestamp = m_start +
            RealTime::frame2RealTime(i * m_stepSize, m_inputSampleRate);
        feature.values[0] = m_df[i];
        feature.label = "";
        fs[DFOutput].push_back(feature);
    }

    for (int i = 1; i < n/2; ++i) {

        // Return the raw autocorrelation in the ACF output, each
        // value labelled according to its corresponding tempo

        feature.timestamp = m_start +
            RealTime::frame2RealTime(i * m_stepSize, m_inputSampleRate);
        feature.values[0] = m_r[i];
        sprintf(buffer, "%.1f bpm", lag2tempo(i));
        if (i == n/2-1) feature.label = "";
        else feature.label = buffer;
        fs[ACFOutput].push_back(feature);
    }

    float t0 = m_minbpm; // our minimum detected tempo
    float t1 = m_maxbpm; // our maximum detected tempo

    int p0 = tempo2lag(t1);
    int p1 = tempo2lag(t0);

    std::map<float, int> candidates;

    for (int i = p0; i <= p1 && i+1 < n/2; ++i) {

        if (m_fr[i] > m_fr[i-1] &&
            m_fr[i] > m_fr[i+1]) {

            // This is a peak in the filtered autocorrelation: stick
            // it into the map from filtered autocorrelation to lag
            // index -- this sorts our peaks by filtered acf value

            candidates[m_fr[i]] = i;
        }

        // Also return the filtered autocorrelation in its own output

        feature.timestamp = m_start +
            RealTime::frame2RealTime(i * m_stepSize, m_inputSampleRate);
        feature.values[0] = m_fr[i];
        sprintf(buffer, "%.1f bpm", lag2tempo(i));
        if (i == p1 || i == n/2-2) feature.label = "";
        else feature.label = buffer;
        fs[FilteredACFOutput].push_back(feature);
    }

    if (candidates.empty()) {
        cerr << "No tempo candidates!" << endl;
        return fs;
    }

    feature.hasTimestamp = true;
    feature.timestamp = m_start;
    
    feature.hasDuration = true;
    feature.duration = m_lasttime - m_start;

    // The map contains only peaks and is sorted by filtered acf
    // value, so the final element in it is our "best" tempo guess

    std::map<float, int>::const_iterator ci = candidates.end();
    --ci;
    int maxpi = ci->second;

    if (m_t[maxpi] > 0) {

        // This lag has an adjusted tempo from the averaging process:
        // use it

        feature.values[0] = m_t[maxpi];

    } else {

        // shouldn't happen -- it would imply that this high value was
        // not a peak!

        feature.values[0] = lag2tempo(maxpi);
        cerr << "WARNING: No stored tempo for index " << maxpi << endl;
    }

    sprintf(buffer, "%.1f bpm", feature.values[0]);
    feature.label = buffer;

    // Return the best tempo in the main output

    fs[TempoOutput].push_back(feature);

    // And return the other estimates (up to the arbitrarily chosen
    // number of 10 of them) in the candidates output

    feature.values.clear();
    feature.label = "";

    while (feature.values.size() < 10) {
        if (m_t[ci->second] > 0) {
            feature.values.push_back(m_t[ci->second]);
        } else {
            feature.values.push_back(lag2tempo(ci->second));
        }
        if (ci == candidates.begin()) break;
        --ci;
    }

    fs[CandidatesOutput].push_back(feature);
    
    return fs;
}

    

FixedTempoEstimator::FixedTempoEstimator(float inputSampleRate) :
    Plugin(inputSampleRate),
    m_d(new D(inputSampleRate))
{
}

FixedTempoEstimator::~FixedTempoEstimator()
{
    delete m_d;
}

string
FixedTempoEstimator::getIdentifier() const
{
    return "fixedtempo";
}

string
FixedTempoEstimator::getName() const
{
    return "Simple Fixed Tempo Estimator";
}

string
FixedTempoEstimator::getDescription() const
{
    return "Study a short section of audio and estimate its tempo, assuming the tempo is constant";
}

string
FixedTempoEstimator::getMaker() const
{
    return "Vamp SDK Example Plugins";
}

int
FixedTempoEstimator::getPluginVersion() const
{
    return 1;
}

string
FixedTempoEstimator::getCopyright() const
{
    return "Code copyright 2008 Queen Mary, University of London.  Freely redistributable (BSD license)";
}

size_t
FixedTempoEstimator::getPreferredStepSize() const
{
    return m_d->getPreferredStepSize();
}

size_t
FixedTempoEstimator::getPreferredBlockSize() const
{
    return m_d->getPreferredBlockSize();
}

bool
FixedTempoEstimator::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) return false;

    return m_d->initialise(channels, stepSize, blockSize);
}

void
FixedTempoEstimator::reset()
{
    return m_d->reset();
}

FixedTempoEstimator::ParameterList
FixedTempoEstimator::getParameterDescriptors() const
{
    return m_d->getParameterDescriptors();
}

float
FixedTempoEstimator::getParameter(std::string id) const
{
    return m_d->getParameter(id);
}

void
FixedTempoEstimator::setParameter(std::string id, float value)
{
    m_d->setParameter(id, value);
}

FixedTempoEstimator::OutputList
FixedTempoEstimator::getOutputDescriptors() const
{
    return m_d->getOutputDescriptors();
}

FixedTempoEstimator::FeatureSet
FixedTempoEstimator::process(const float *const *inputBuffers, RealTime ts)
{
    return m_d->process(inputBuffers, ts);
}

FixedTempoEstimator::FeatureSet
FixedTempoEstimator::getRemainingFeatures()
{
    return m_d->getRemainingFeatures();
}
