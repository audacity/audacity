/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TestWaveClipMaker.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioContainer.h"
#include "SampleFormat.h"
#include "WaveClip.h"
#include "WaveTrack.h"

class SampleBlockFactory;

class TestWaveClipMaker final
{
public:
    TestWaveClipMaker(int sampleRate, SampleBlockFactoryPtr);

    using Operations = std::function<void (WaveClip&)>;

    WaveClipHolder ClipFilledWith(
        const std::vector<std::vector<float> >& values, Operations operations = [] (WaveClip&) {}) const;

    WaveClipHolder ClipFilledWith(
        const std::vector<float>& values, size_t numChannels, Operations operations = [] (WaveClip&) {}) const;

    WaveClipHolder ClipFilledWith(
        float value, size_t numValues, size_t numChannels, Operations operations = [] (WaveClip&) {}) const;

private:
    static constexpr bool copyCutLines = false;

    const int mSampleRate;
    const SampleBlockFactoryPtr mFactory;
};
