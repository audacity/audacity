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

#ifndef _CONSTANT_Q_SPECTROGRAM_PLUGIN_H_
#define _CONSTANT_Q_SPECTROGRAM_PLUGIN_H_

#include <vamp-sdk/Plugin.h>
#include <dsp/chromagram/ConstantQ.h>

#include <queue>

class ConstantQSpectrogram : public Vamp::Plugin
{
public:
    ConstantQSpectrogram(float inputSampleRate);
    virtual ~ConstantQSpectrogram();

    bool initialise(size_t channels, size_t stepSize, size_t blockSize);
    void reset();

    InputDomain getInputDomain() const { return FrequencyDomain; }

    std::string getIdentifier() const;
    std::string getName() const;
    std::string getDescription() const;
    std::string getMaker() const;
    int getPluginVersion() const;
    std::string getCopyright() const;

    ParameterList getParameterDescriptors() const;
    float getParameter(std::string) const;
    void setParameter(std::string, float);

    size_t getPreferredStepSize() const;
    size_t getPreferredBlockSize() const;

    OutputList getOutputDescriptors() const;

    FeatureSet process(const float *const *inputBuffers,
                       Vamp::RealTime timestamp);

    FeatureSet getRemainingFeatures();

protected:
    int m_minMIDIPitch;
    int m_maxMIDIPitch;
    float m_tuningFrequency;
    bool m_normalized;
    int m_bpo;
    int m_bins;

    void setupConfig();

    CQConfig m_config;
    ConstantQ *m_cq;
    mutable size_t m_step;
    mutable size_t m_block;

    Feature normalize(const Feature &);
};


#endif
