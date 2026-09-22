/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
 * SimilarityPlugin.cpp
 *
 * Copyright 2009 Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
 */

#include <iostream>
#include <cstdio>
#include <algorithm>

#include "SimilarityPlugin.h"
#include "base/Pitch.h"
#include "dsp/mfcc/MFCC.h"
#include "dsp/chromagram/Chromagram.h"
#include "dsp/rateconversion/Decimator.h"
#include "dsp/rhythm/BeatSpectrum.h"
#include "maths/KLDivergence.h"
#include "maths/CosineDistance.h"
#include "maths/MathUtilities.h"

using std::string;
using std::vector;
using std::cerr;
using std::endl;
using std::ostringstream;

const float
SimilarityPlugin::m_noRhythm = 0.009;

const float
SimilarityPlugin::m_allRhythm = 0.991;

SimilarityPlugin::SimilarityPlugin(float inputSampleRate) :
    Plugin(inputSampleRate),
    m_type(TypeMFCC),
    m_mfcc(0),
    m_rhythmfcc(0),
    m_chromagram(0),
    m_decimator(0),
    m_featureColumnSize(20),
    m_rhythmWeighting(0.5f),
    m_rhythmClipDuration(4.f), // seconds
    m_rhythmClipOrigin(40.f), // seconds
    m_rhythmClipFrameSize(0),
    m_rhythmClipFrames(0),
    m_rhythmColumnSize(20),
    m_blockSize(0),
    m_channels(0),
    m_processRate(0),
    m_frameNo(0),
    m_done(false)
{
    int rate = lrintf(m_inputSampleRate);
    int internalRate = 22050;
    int decimationFactor = rate / internalRate;
    if (decimationFactor < 1) decimationFactor = 1;

    // must be a power of two
    while (decimationFactor & (decimationFactor - 1)) ++decimationFactor;

    m_processRate = rate / decimationFactor; // may be 22050, 24000 etc
}

SimilarityPlugin::~SimilarityPlugin()
{
    delete m_mfcc;
    delete m_rhythmfcc;
    delete m_chromagram;
    delete m_decimator;
}

string
SimilarityPlugin::getIdentifier() const
{
    return "qm-similarity";
}

string
SimilarityPlugin::getName() const
{
    return "Similarity";
}

string
SimilarityPlugin::getDescription() const
{
    return "Return a distance matrix for similarity between the input audio channels";
}

string
SimilarityPlugin::getMaker() const
{
    return "Queen Mary, University of London";
}

int
SimilarityPlugin::getPluginVersion() const
{
    return 1;
}

string
SimilarityPlugin::getCopyright() const
{
    return "Plugin by Mark Levy, Kurt Jacobson and Chris Cannam.  Copyright (c) 2009 QMUL - All Rights Reserved";
}

size_t
SimilarityPlugin::getMinChannelCount() const
{
    return 1;
}

size_t
SimilarityPlugin::getMaxChannelCount() const
{
    return 1024;
}

int
SimilarityPlugin::getDecimationFactor() const
{
    int rate = lrintf(m_inputSampleRate);
    return rate / m_processRate;
}

size_t
SimilarityPlugin::getPreferredStepSize() const
{
    if (m_blockSize == 0) calculateBlockSize();

    // there is also an assumption to this effect in process()
    // (referring to m_fftSize/2 instead of a literal post-decimation
    // step size):
    return m_blockSize/2;
}

size_t
SimilarityPlugin::getPreferredBlockSize() const
{
    if (m_blockSize == 0) calculateBlockSize();
    return m_blockSize;
}

void
SimilarityPlugin::calculateBlockSize() const
{
    if (m_blockSize != 0) return;
    int decimationFactor = getDecimationFactor();
    m_blockSize = 2048 * decimationFactor;
}

SimilarityPlugin::ParameterList SimilarityPlugin::getParameterDescriptors() const
{
    ParameterList list;

    ParameterDescriptor desc;
    desc.identifier = "featureType";
    desc.name = "Feature Type";
    desc.description = "Audio feature used for similarity measure.  Timbral: use the first 20 MFCCs (19 plus C0).  Chromatic: use 12 bin-per-octave chroma.  Rhythmic: compare beat spectra of short regions.";
    desc.unit = "";
    desc.minValue = 0;
    desc.maxValue = 4;
    desc.defaultValue = 1;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    desc.valueNames.push_back("Timbre");
    desc.valueNames.push_back("Timbre and Rhythm");
    desc.valueNames.push_back("Chroma");
    desc.valueNames.push_back("Chroma and Rhythm");
    desc.valueNames.push_back("Rhythm only");
    list.push_back(desc);	
/*
    desc.identifier = "rhythmWeighting";
    desc.name = "Influence of Rhythm";
    desc.description = "Proportion of similarity measure made up from rhythmic similarity component, from 0 (entirely timbral or chromatic) to 100 (entirely rhythmic).";
    desc.unit = "%";
    desc.minValue = 0;
    desc.maxValue = 100;
    desc.defaultValue = 0;
    desc.isQuantized = false;
    desc.valueNames.clear();
    list.push_back(desc);	
*/
    return list;
}

float
SimilarityPlugin::getParameter(std::string param) const
{
    if (param == "featureType") {

        if (m_rhythmWeighting > m_allRhythm) {
            return 4;
        }

        switch (m_type) {

        case TypeMFCC:
            if (m_rhythmWeighting < m_noRhythm) return 0;
            else return 1;
            break;

        case TypeChroma:
            if (m_rhythmWeighting < m_noRhythm) return 2;
            else return 3;
            break;
        }            

        return 1;

//    } else if (param == "rhythmWeighting") {
//        return nearbyint(m_rhythmWeighting * 100.0);
    }

    std::cerr << "WARNING: SimilarityPlugin::getParameter: unknown parameter \""
              << param << "\"" << std::endl;
    return 0.0;
}

void
SimilarityPlugin::setParameter(std::string param, float value)
{
    if (param == "featureType") {

        int v = int(value + 0.1);

        Type newType = m_type;

        switch (v) {
        case 0: newType = TypeMFCC; m_rhythmWeighting = 0.0f; break;
        case 1: newType = TypeMFCC; m_rhythmWeighting = 0.5f; break;
        case 2: newType = TypeChroma; m_rhythmWeighting = 0.0f; break;
        case 3: newType = TypeChroma; m_rhythmWeighting = 0.5f; break;
        case 4: newType = TypeMFCC; m_rhythmWeighting = 1.f; break;
        }

        if (newType != m_type) m_blockSize = 0;

        m_type = newType;
        return;

//    } else if (param == "rhythmWeighting") {
//        m_rhythmWeighting = value / 100;
//        return;
    }

    std::cerr << "WARNING: SimilarityPlugin::setParameter: unknown parameter \""
              << param << "\"" << std::endl;
}

SimilarityPlugin::OutputList
SimilarityPlugin::getOutputDescriptors() const
{
    OutputList list;
	
    OutputDescriptor similarity;
    similarity.identifier = "distancematrix";
    similarity.name = "Distance Matrix";
    similarity.description = "Distance matrix for similarity metric.  Smaller = more similar.  Should be symmetrical.";
    similarity.unit = "";
    similarity.hasFixedBinCount = true;
    similarity.binCount = m_channels;
    similarity.hasKnownExtents = false;
    similarity.isQuantized = false;
    similarity.sampleType = OutputDescriptor::FixedSampleRate;
    similarity.sampleRate = 1;
	
    m_distanceMatrixOutput = list.size();
    list.push_back(similarity);
	
    OutputDescriptor simvec;
    simvec.identifier = "distancevector";
    simvec.name = "Distance from First Channel";
    simvec.description = "Distance vector for similarity of each channel to the first channel.  Smaller = more similar.";
    simvec.unit = "";
    simvec.hasFixedBinCount = true;
    simvec.binCount = m_channels;
    simvec.hasKnownExtents = false;
    simvec.isQuantized = false;
    simvec.sampleType = OutputDescriptor::FixedSampleRate;
    simvec.sampleRate = 1;
	
    m_distanceVectorOutput = list.size();
    list.push_back(simvec);
	
    OutputDescriptor sortvec;
    sortvec.identifier = "sorteddistancevector";
    sortvec.name = "Ordered Distances from First Channel";
    sortvec.description = "Vector of the order of other channels in similarity to the first, followed by distance vector for similarity of each to the first.  Smaller = more similar.";
    sortvec.unit = "";
    sortvec.hasFixedBinCount = true;
    sortvec.binCount = m_channels;
    sortvec.hasKnownExtents = false;
    sortvec.isQuantized = false;
    sortvec.sampleType = OutputDescriptor::FixedSampleRate;
    sortvec.sampleRate = 1;
	
    m_sortedVectorOutput = list.size();
    list.push_back(sortvec);
	
    OutputDescriptor means;
    means.identifier = "means";
    means.name = "Feature Means";
    means.description = "Means of the feature bins.  Feature time (sec) corresponds to input channel.  Number of bins depends on selected feature type.";
    means.unit = "";
    means.hasFixedBinCount = true;
    means.binCount = m_featureColumnSize;
    means.hasKnownExtents = false;
    means.isQuantized = false;
    means.sampleType = OutputDescriptor::FixedSampleRate;
    means.sampleRate = 1;
	
    m_meansOutput = list.size();
    list.push_back(means);
    
    OutputDescriptor variances;
    variances.identifier = "variances";
    variances.name = "Feature Variances";
    variances.description = "Variances of the feature bins.  Feature time (sec) corresponds to input channel.  Number of bins depends on selected feature type.";
    variances.unit = "";
    variances.hasFixedBinCount = true;
    variances.binCount = m_featureColumnSize;
    variances.hasKnownExtents = false;
    variances.isQuantized = false;
    variances.sampleType = OutputDescriptor::FixedSampleRate;
    variances.sampleRate = 1;
	
    m_variancesOutput = list.size();
    list.push_back(variances);
    
    OutputDescriptor beatspectrum;
    beatspectrum.identifier = "beatspectrum";
    beatspectrum.name = "Beat Spectra";
    beatspectrum.description = "Rhythmic self-similarity vectors (beat spectra) for the input channels.  Feature time (sec) corresponds to input channel.  Not returned if rhythm weighting is zero.";
    beatspectrum.unit = "";
    if (m_rhythmClipFrames > 0) {
        beatspectrum.hasFixedBinCount = true;
        beatspectrum.binCount = m_rhythmClipFrames / 2;
    } else {
        beatspectrum.hasFixedBinCount = false;
    }
    beatspectrum.hasKnownExtents = false;
    beatspectrum.isQuantized = false;
    beatspectrum.sampleType = OutputDescriptor::FixedSampleRate;
    beatspectrum.sampleRate = 1;
	
    m_beatSpectraOutput = list.size();
    list.push_back(beatspectrum);
    
    return list;
}

bool
SimilarityPlugin::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (channels < getMinChannelCount()) return false;

    // Using more than getMaxChannelCount is not actually a problem
    // for us.  Using "incorrect" step and block sizes would be fine
    // for timbral or chroma similarity, but will break rhythmic
    // similarity, so we'd better enforce these.

    if (stepSize != getPreferredStepSize()) {
        std::cerr << "SimilarityPlugin::initialise: supplied step size "
                  << stepSize << " differs from required step size "
                  << getPreferredStepSize() << std::endl;
        return false;
    }

    if (blockSize != getPreferredBlockSize()) {
        std::cerr << "SimilarityPlugin::initialise: supplied block size "
                  << blockSize << " differs from required block size "
                  << getPreferredBlockSize() << std::endl;
        return false;
    }        
    
    m_blockSize = blockSize;
    m_channels = channels;

    m_lastNonEmptyFrame = std::vector<int>(m_channels);
    for (int i = 0; i < m_channels; ++i) m_lastNonEmptyFrame[i] = -1;

    m_emptyFrameCount = std::vector<int>(m_channels);
    for (int i = 0; i < m_channels; ++i) m_emptyFrameCount[i] = 0;

    m_frameNo = 0;

    int decimationFactor = getDecimationFactor();
    if (decimationFactor > 1) {
        m_decimator = new Decimator(m_blockSize, decimationFactor);
    }

    if (m_type == TypeMFCC) {

        m_featureColumnSize = 20;

        MFCCConfig config(m_processRate);
        config.fftsize = 2048;
        config.nceps = m_featureColumnSize - 1;
        config.want_c0 = true;
        config.logpower = 1;
        m_mfcc = new MFCC(config);
        m_fftSize = m_mfcc->getfftlength();
        m_rhythmClipFrameSize = m_fftSize / 4;

//        std::cerr << "MFCC FS = " << config.FS << ", FFT size = " << m_fftSize<< std::endl;

    } else if (m_type == TypeChroma) {

        m_featureColumnSize = 12;

        // For simplicity, aim to have the chroma fft size equal to
        // 2048, the same as the mfcc fft size (so the input block
        // size does not depend on the feature type and we can use the
        // same processing parameters for rhythm etc).  This is also
        // why getPreferredBlockSize can confidently return 2048 * the
        // decimation factor.
        
        // The fft size for a chromagram is the filterbank Q value
        // times the sample rate, divided by the minimum frequency,
        // rounded up to the nearest power of two.

        double q = 1.0 / (pow(2.0, (1.0 / 12.0)) - 1.0);
        double fmin = (q * m_processRate) / 2048.0;

        // Round fmin up to the nearest MIDI pitch multiple of 12.
        // So long as fmin is greater than 12 to start with, this
        // should not change the resulting fft size.

        int pmin = Pitch::getPitchForFrequency(float(fmin));
        pmin = ((pmin / 12) + 1) * 12;
        fmin = Pitch::getFrequencyForPitch(pmin);

        float fmax = Pitch::getFrequencyForPitch(pmin + 36);

        ChromaConfig config;
        config.FS = m_processRate;
        config.min = fmin;
        config.max = fmax;
        config.BPO = 12;
        config.CQThresh = 0.0054;
        // We don't normalise the chromagram's columns individually;
        // we normalise the mean at the end instead
        config.normalise = MathUtilities::NormaliseNone;
        m_chromagram = new Chromagram(config);
        m_fftSize = m_chromagram->getFrameSize();
        
        if (m_fftSize != 2048) {
            std::cerr << "WARNING: SimilarityPlugin::initialise: Internal processing FFT size " << m_fftSize << " != expected size 2048 in chroma mode" << std::endl;
        }

//        std::cerr << "fftsize = " << m_fftSize << std::endl;

        m_rhythmClipFrameSize = m_fftSize / 4;

//        std::cerr << "m_rhythmClipFrameSize = " << m_rhythmClipFrameSize << std::endl;
//        std::cerr << "min = "<< config.min << ", max = " << config.max << std::endl;

    } else {

        std::cerr << "SimilarityPlugin::initialise: internal error: unknown type " << m_type << std::endl;
        return false;
    }
    
    if (needRhythm()) {
        m_rhythmClipFrames =
            int(ceil((m_rhythmClipDuration * m_processRate) 
                     / m_rhythmClipFrameSize));
//        std::cerr << "SimilarityPlugin::initialise: rhythm clip requires "
//                  << m_rhythmClipFrames << " frames of size "
//                  << m_rhythmClipFrameSize << " at process rate "
//                  << m_processRate << " ( = "
//                  << (float(m_rhythmClipFrames * m_rhythmClipFrameSize) / m_processRate) << " sec )"
//                  << std::endl;

        MFCCConfig config(m_processRate);
        config.fftsize = m_rhythmClipFrameSize;
        config.nceps = m_rhythmColumnSize - 1;
        config.want_c0 = true;
        config.logpower = 1;
        config.window = RectangularWindow; // because no overlap
        m_rhythmfcc = new MFCC(config);
    }

    for (int i = 0; i < m_channels; ++i) {

        m_values.push_back(FeatureMatrix());

        if (needRhythm()) {
            m_rhythmValues.push_back(FeatureColumnQueue());
        }
    }

    m_done = false;

    return true;
}

void
SimilarityPlugin::reset()
{
    for (int i = 0; i < int(m_values.size()); ++i) {
        m_values[i].clear();
    }

    for (int i = 0; i < int(m_rhythmValues.size()); ++i) {
        m_rhythmValues[i].clear();
    }

    for (int i = 0; i < int(m_lastNonEmptyFrame.size()); ++i) {
        m_lastNonEmptyFrame[i] = -1;
    }

    for (int i = 0; i < int(m_emptyFrameCount.size()); ++i) {
        m_emptyFrameCount[i] = 0;
    }

    m_done = false;
}

SimilarityPlugin::FeatureSet
SimilarityPlugin::process(const float *const *inputBuffers, Vamp::RealTime /* timestamp */)
{
    if (m_done) {
        return FeatureSet();
    }

    double *dblbuf = new double[m_blockSize];
    double *decbuf = dblbuf;
    if (m_decimator) decbuf = new double[m_fftSize];

    double *raw = new double[std::max(m_featureColumnSize,
                                      m_rhythmColumnSize)];

    float threshold = 1e-10;

    bool someRhythmFrameNeeded = false;

    for (int c = 0; c < m_channels; ++c) {

        bool empty = true;

        for (int i = 0; i < m_blockSize; ++i) {
            float val = inputBuffers[c][i];
            if (fabs(val) > threshold) empty = false;
            dblbuf[i] = val;
        }

        if (empty) {
            if (needRhythm() && ((m_frameNo % 2) == 0)) {
                for (int i = 0; i < m_fftSize / m_rhythmClipFrameSize; ++i) {
                    if (int(m_rhythmValues[c].size()) < m_rhythmClipFrames) {
                        FeatureColumn mf(m_rhythmColumnSize);
                        for (int i = 0; i < m_rhythmColumnSize; ++i) {
                            mf[i] = 0.0;
                        }
                        m_rhythmValues[c].push_back(mf);
                    }
                }
            }
            m_emptyFrameCount[c]++;
            continue;
        }

        m_lastNonEmptyFrame[c] = m_frameNo;

        if (m_decimator) {
            m_decimator->process(dblbuf, decbuf);
        }

        if (needTimbre()) {

            FeatureColumn mf(m_featureColumnSize);

            if (m_type == TypeMFCC) {
                m_mfcc->process(decbuf, raw);
                for (int i = 0; i < m_featureColumnSize; ++i) {
                    mf[i] = raw[i];
                }
            } else if (m_type == TypeChroma) {
                double *chroma = m_chromagram->process(decbuf);
                for (int i = 0; i < m_featureColumnSize; ++i) {
                    mf[i] = chroma[i];
                }
            }
        
            m_values[c].push_back(mf);
        }

//        std::cerr << "needRhythm = " << needRhythm() << ", frame = " << m_frameNo << std::endl;

        if (needRhythm() && ((m_frameNo % 2) == 0)) {

            // The incoming frames are overlapping; we only use every
            // other one, because we don't want the overlap (it would
            // screw up the rhythm)

            int frameOffset = 0;

            while (frameOffset + m_rhythmClipFrameSize <= m_fftSize) {

                bool needRhythmFrame = true;

                if (int(m_rhythmValues[c].size()) >= m_rhythmClipFrames) {

                    needRhythmFrame = false;

                    // assumes hopsize = framesize/2
                    float current = m_frameNo * (m_fftSize/2) + frameOffset;
                    current = current / m_processRate;
                    if (current - m_rhythmClipDuration < m_rhythmClipOrigin) {
                        needRhythmFrame = true;
                        m_rhythmValues[c].pop_front();
                    }

//                    if (needRhythmFrame) {
//                        std::cerr << "at current = " <<current << " (frame = " << m_frameNo << "), have " << m_rhythmValues[c].size() << ", need rhythm = " << needRhythmFrame << std::endl;
//                    }

                }
                
                if (needRhythmFrame) {

                    someRhythmFrameNeeded = true;

                    m_rhythmfcc->process(decbuf + frameOffset, raw);
                    
                    FeatureColumn mf(m_rhythmColumnSize);
                    for (int i = 0; i < m_rhythmColumnSize; ++i) {
                        mf[i] = raw[i];
                    }

                    m_rhythmValues[c].push_back(mf);
                }

                frameOffset += m_rhythmClipFrameSize;
            }
        }
    }

    if (!needTimbre() && !someRhythmFrameNeeded && ((m_frameNo % 2) == 0)) {
//        std::cerr << "done!" << std::endl;
        m_done = true;
    }

    if (m_decimator) delete[] decbuf;
    delete[] dblbuf;
    delete[] raw;
	
    ++m_frameNo;

    return FeatureSet();
}

SimilarityPlugin::FeatureMatrix
SimilarityPlugin::calculateTimbral(FeatureSet &returnFeatures)
{
    FeatureMatrix m(m_channels); // means
    FeatureMatrix v(m_channels); // variances
    
    for (int i = 0; i < m_channels; ++i) {

        FeatureColumn mean(m_featureColumnSize), variance(m_featureColumnSize);

        for (int j = 0; j < m_featureColumnSize; ++j) {

            mean[j] = 0.0;
            variance[j] = 0.0;
            int count;

            // We want to take values up to, but not including, the
            // last non-empty frame (which may be partial)

            int sz = m_lastNonEmptyFrame[i] - m_emptyFrameCount[i];
            if (sz < 0) sz = 0;
            if (sz >= int(m_values[i].size())) {
                sz = int(m_values[i].size())-1;
            }

            count = 0;
            for (int k = 0; k < sz; ++k) {
                double val = m_values[i][k][j];
                if (ISNAN(val) || ISINF(val)) continue;
                mean[j] += val;
                ++count;
            }
            if (count > 0) mean[j] /= count;

            count = 0;
            for (int k = 0; k < sz; ++k) {
                double val = ((m_values[i][k][j] - mean[j]) *
                              (m_values[i][k][j] - mean[j]));
                if (ISNAN(val) || ISINF(val)) continue;
                variance[j] += val;
                ++count;
            }
            if (count > 0) variance[j] /= count;
        }

        m[i] = mean;
        v[i] = variance;
    }

    FeatureMatrix distances(m_channels);

    if (m_type == TypeMFCC) {

        // "Despite the fact that MFCCs extracted from music are
        // clearly not Gaussian, [14] showed, somewhat surprisingly,
        // that a similarity function comparing single Gaussians
        // modelling MFCCs for each track can perform as well as
        // mixture models.  A great advantage of using single
        // Gaussians is that a simple closed form exists for the KL
        // divergence." -- Mark Levy, "Lightweight measures for
        // timbral similarity of musical audio"
        // (http://www.elec.qmul.ac.uk/easaier/papers/mlevytimbralsimilarity.pdf)

        KLDivergence kld;

        for (int i = 0; i < m_channels; ++i) {
            for (int j = 0; j < m_channels; ++j) {
                double d = kld.distanceGaussian(m[i], v[i], m[j], v[j]);
                distances[i].push_back(d);
            }
        }

    } else {

        // We use the KL divergence for distributions of discrete
        // variables, as chroma are histograms already.  Or at least,
        // they will be when we've normalised them like this:
        for (int i = 0; i < m_channels; ++i) {
            MathUtilities::normalise(m[i], MathUtilities::NormaliseUnitSum);
        }

        KLDivergence kld;

        for (int i = 0; i < m_channels; ++i) {
            for (int j = 0; j < m_channels; ++j) {
                double d = kld.distanceDistribution(m[i], m[j], true);
                distances[i].push_back(d);
            }
        }
    }
    
    Feature feature;
    feature.hasTimestamp = true;

    char labelBuffer[100];

    for (int i = 0; i < m_channels; ++i) {

        feature.timestamp = Vamp::RealTime(i, 0);

        sprintf(labelBuffer, "Means for channel %d", i+1);
        feature.label = labelBuffer;

        feature.values.clear();
        for (int k = 0; k < m_featureColumnSize; ++k) {
            feature.values.push_back(m[i][k]);
        }

        returnFeatures[m_meansOutput].push_back(feature);

        sprintf(labelBuffer, "Variances for channel %d", i+1);
        feature.label = labelBuffer;

        feature.values.clear();
        for (int k = 0; k < m_featureColumnSize; ++k) {
            feature.values.push_back(v[i][k]);
        }

        returnFeatures[m_variancesOutput].push_back(feature);
    }

    return distances;
}

SimilarityPlugin::FeatureMatrix
SimilarityPlugin::calculateRhythmic(FeatureSet &returnFeatures)
{
    if (!needRhythm()) return FeatureMatrix();

//        std::cerr << "SimilarityPlugin::initialise: rhythm clip for channel 0 contains "
//                  << m_rhythmValues[0].size() << " frames of size "
//                  << m_rhythmClipFrameSize << " at process rate "
//                  << m_processRate << " ( = "
//                  << (float(m_rhythmValues[0].size() * m_rhythmClipFrameSize) / m_processRate) << " sec )"
//                  << std::endl;

    BeatSpectrum bscalc;
    CosineDistance cd;

    // Our rhythm feature matrix is a deque of vectors for practical
    // reasons, but BeatSpectrum::process wants a vector of vectors
    // (which is what FeatureMatrix happens to be).

    FeatureMatrixSet bsinput(m_channels);
    for (int i = 0; i < m_channels; ++i) {
        for (int j = 0; j < int(m_rhythmValues[i].size()); ++j) {
            bsinput[i].push_back(m_rhythmValues[i][j]);
        }
    }

    FeatureMatrix bs(m_channels);
    for (int i = 0; i < m_channels; ++i) {
        bs[i] = bscalc.process(bsinput[i]);
    }

    FeatureMatrix distances(m_channels);
    for (int i = 0; i < m_channels; ++i) {
        for (int j = 0; j < m_channels; ++j) {
            double d = cd.distance(bs[i], bs[j]);
            distances[i].push_back(d);
        }
    }

    Feature feature;
    feature.hasTimestamp = true;

    char labelBuffer[100];

    for (int i = 0; i < m_channels; ++i) {

        feature.timestamp = Vamp::RealTime(i, 0);

        sprintf(labelBuffer, "Beat spectrum for channel %d", i+1);
        feature.label = labelBuffer;

        feature.values.clear();
        for (int j = 0; j < int(bs[i].size()); ++j) {
            feature.values.push_back(bs[i][j]);
        }

        returnFeatures[m_beatSpectraOutput].push_back(feature);
    }

    return distances;
}

double
SimilarityPlugin::getDistance(const FeatureMatrix &timbral,
                              const FeatureMatrix &rhythmic,
                              int i, int j)
{
    double distance = 1.0;
    if (needTimbre()) distance *= timbral[i][j];
    if (needRhythm()) distance *= rhythmic[i][j];
    return distance;
}

SimilarityPlugin::FeatureSet
SimilarityPlugin::getRemainingFeatures()
{
    FeatureSet returnFeatures;

    // We want to return a matrix of the distances between channels,
    // but Vamp doesn't have a matrix return type so we will actually
    // return a series of vectors

    FeatureMatrix timbralDistances, rhythmicDistances;

    if (needTimbre()) {
        timbralDistances = calculateTimbral(returnFeatures);
    }

    if (needRhythm()) {
        rhythmicDistances = calculateRhythmic(returnFeatures);
    }
    
    // We give all features a timestamp, otherwise hosts will tend to
    // stamp them at the end of the file, which is annoying

    Feature feature;
    feature.hasTimestamp = true;

    Feature distanceVectorFeature;
    distanceVectorFeature.label = "Distance from first channel";
    distanceVectorFeature.hasTimestamp = true;
    distanceVectorFeature.timestamp = Vamp::RealTime::zeroTime;

    std::map<double, int> sorted;

    char labelBuffer[100];

    for (int i = 0; i < m_channels; ++i) {

        feature.timestamp = Vamp::RealTime(i, 0);

        feature.values.clear();
        for (int j = 0; j < m_channels; ++j) {
            double dist = getDistance(timbralDistances, rhythmicDistances, i, j);
            feature.values.push_back(dist);
        }

        sprintf(labelBuffer, "Distances from channel %d", i+1);
        feature.label = labelBuffer;
		
        returnFeatures[m_distanceMatrixOutput].push_back(feature);

        double fromFirst = 
            getDistance(timbralDistances, rhythmicDistances, 0, i);

        distanceVectorFeature.values.push_back(fromFirst);
        sorted[fromFirst] = i;
    }

    returnFeatures[m_distanceVectorOutput].push_back(distanceVectorFeature);

    feature.label = "Order of channels by similarity to first channel";
    feature.values.clear();
    feature.timestamp = Vamp::RealTime(0, 0);

    for (std::map<double, int>::iterator i = sorted.begin();
         i != sorted.end(); ++i) {
        feature.values.push_back(i->second + 1);
    }

    returnFeatures[m_sortedVectorOutput].push_back(feature);

    feature.label = "Ordered distances of channels from first channel";
    feature.values.clear();
    feature.timestamp = Vamp::RealTime(1, 0);

    for (std::map<double, int>::iterator i = sorted.begin();
         i != sorted.end(); ++i) {
        feature.values.push_back(i->first);
    }

    returnFeatures[m_sortedVectorOutput].push_back(feature);

    return returnFeatures;
}
