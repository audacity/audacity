/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TestWaveClipMaker.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "au3-time-and-pitch/AudioContainer.h"
#include "au3-math/SampleFormat.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"

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
