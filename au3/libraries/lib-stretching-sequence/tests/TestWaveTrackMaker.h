/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TestWaveTrackMaker.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "MockSampleBlockFactory.h"
#include "SampleFormat.h"
#include "WaveClip.h"
#include "WaveTrack.h"

class TestWaveTrackMaker final
{
public:
    TestWaveTrackMaker(int sampleRate, SampleBlockFactoryPtr);
    std::shared_ptr<WaveTrack> Track(const WaveClipHolders& clips) const;
    std::shared_ptr<WaveTrack> Track(const WaveClipHolder& clip) const;

private:
    const int mSampleRate;
    const SampleBlockFactoryPtr mFactory;
};
