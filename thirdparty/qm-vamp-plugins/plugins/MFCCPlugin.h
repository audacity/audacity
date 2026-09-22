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

#ifndef _MFCC_PLUGIN_H_
#define _MFCC_PLUGIN_H_

#include <vamp-sdk/Plugin.h>
#include <dsp/mfcc/MFCC.h>

#include <vector>

class MFCCPlugin : public Vamp::Plugin
{
public:
    MFCCPlugin(float inputSampleRate);
    virtual ~MFCCPlugin();

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
    int m_bins; // == nceps is m_wantC0 false or nceps+1 if m_wantC0 true
    bool m_wantC0;
    float m_logpower;

    void setupConfig();

    MFCCConfig m_config;
    MFCC *m_mfcc;
    mutable size_t m_step;
    mutable size_t m_block;

    std::vector<double> m_binsums;
    size_t m_count;

    Feature normalize(const Feature &);
};


#endif
