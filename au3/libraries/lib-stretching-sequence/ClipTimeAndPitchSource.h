/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipTimeAndPitchSource.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioSegmentSampleView.h"
#include "PlaybackDirection.h"
#include "SampleCount.h"
#include "TimeAndPitchInterface.h"

#include <memory>

class ClipInterface;
using ChannelSampleViews = std::vector<AudioSegmentSampleView>;

class STRETCHING_SEQUENCE_API ClipTimeAndPitchSource final : public TimeAndPitchSource
{
public:
    ClipTimeAndPitchSource(
        const ClipInterface&, double durationToDiscard, PlaybackDirection);

    // TimeAndPitchSource
    void Pull(float* const*, size_t samplesPerChannel) override;

    size_t NChannels() const;

private:
    const ClipInterface& mClip;
    sampleCount mLastReadSample = 0;
    const PlaybackDirection mPlaybackDirection;
    ChannelSampleViews mChannelSampleViews;
};
