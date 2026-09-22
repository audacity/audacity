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

#ifndef _CHROMAGRAM_PLUGIN_H_
#define _CHROMAGRAM_PLUGIN_H_

#include <vamp-sdk/Plugin.h>
#include <dsp/chromagram/Chromagram.h>

class ChromagramPlugin : public Vamp::Plugin
{
public:
    ChromagramPlugin(float inputSampleRate);
    virtual ~ChromagramPlugin();

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
    MathUtilities::NormaliseType m_normalise;
    int m_bpo;

    void setupConfig();

    ChromaConfig m_config;
    Chromagram *m_chromagram;
    mutable size_t m_step;
    mutable size_t m_block;

    std::vector<double> m_binsums;
    size_t m_count;
};


#endif
