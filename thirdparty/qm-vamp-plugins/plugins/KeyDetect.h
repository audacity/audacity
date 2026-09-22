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

#ifndef _GETMODE_PLUGIN_H_
#define _GETMODE_PLUGIN_H_

#include <vamp-sdk/Plugin.h>

#include <dsp/keydetection/GetKeyMode.h>

class KeyDetector : public Vamp::Plugin
{
public:
    KeyDetector(float inputSampleRate);
    virtual ~KeyDetector();

    bool initialise(size_t channels, size_t stepSize, size_t blockSize);
    void reset();

    InputDomain getInputDomain() const { return TimeDomain; }

    std::string getIdentifier() const;
    std::string getName() const;
    std::string getDescription() const;
    std::string getMaker() const;
    int getPluginVersion() const;
    std::string getCopyright() const;

    ParameterList getParameterDescriptors() const;
    float getParameter(std::string) const;
    void setParameter(std::string, float);

    OutputList getOutputDescriptors() const;

    FeatureSet process(const float *const *inputBuffers,
                       Vamp::RealTime timestamp);

    FeatureSet getRemainingFeatures();

    size_t getPreferredStepSize() const;
    size_t getPreferredBlockSize() const;

protected:
    mutable size_t m_stepSize;
    mutable size_t m_blockSize;
    float m_tuningFrequency;
    int m_length;

    GetKeyMode::Config getConfig() const;
    std::string getKeyName(int index, bool minor, bool includeMajMin) const;
    std::string getBothKeyNames(int index) const;

    GetKeyMode* m_getKeyMode;
    double* m_inputFrame;
    int m_prevKey;
    bool m_first;
};


#endif
