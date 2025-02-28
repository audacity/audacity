/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TestWaveClipMaker.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "TestWaveClipMaker.h"
#include "AudioContainerHelper.h"
#include "MockSampleBlockFactory.h"

TestWaveClipMaker::TestWaveClipMaker(
    int sampleRate, SampleBlockFactoryPtr factory)
    : mSampleRate{sampleRate}
    , mFactory{factory}
{
}

WaveClipHolder TestWaveClipMaker::ClipFilledWith(
    const std::vector<std::vector<float> >& values, Operations operations) const
{
    const auto numSamples = values[0].size();
    const auto clip = std::make_shared<WaveClip>(
        values.size(), mFactory, floatSample, mSampleRate);
    // Is there any more convenient way of doing this ?
    clip->InsertSilence(0, 1. * numSamples / mSampleRate);
    for (auto i = 0u; i < values.size(); ++i) {
        AudioContainer audio(numSamples, 1u);
        std::copy(values[i].begin(), values[i].end(), audio.Get()[0u]);
        clip->SetSamples(
            i, AudioContainerHelper::GetData<char>(audio)[0u], floatSample, 0,
            numSamples, floatSample);
    }
    operations(*clip);
    return clip;
}

WaveClipHolder TestWaveClipMaker::ClipFilledWith(
    const std::vector<float>& values, size_t numChannels,
    Operations operations) const
{
    return ClipFilledWith(
        std::vector<std::vector<float> > { values }, operations);
}

WaveClipHolder TestWaveClipMaker::ClipFilledWith(
    float value, size_t numValues, size_t numChannels,
    Operations operations) const
{
    std::vector<float> values(numValues);
    std::fill(values.begin(), values.end(), value);
    return ClipFilledWith(values, numChannels, operations);
}
