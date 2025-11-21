/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioSegmentFactory.h

  Matthieu Hodgkinson

**********************************************************************/

#pragma once

#include "AudioSegmentFactoryInterface.h"
#include "ClipInterface.h"
#include "TimeAndPitchInterface.h"

#include <memory>

class ClipInterface;
using ClipConstHolders = std::vector<std::shared_ptr<const ClipInterface> >;

class STRETCHING_SEQUENCE_API AudioSegmentFactory final : public AudioSegmentFactoryInterface
{
public:
    AudioSegmentFactory(int sampleRate, int numChannels, ClipConstHolders clips);

    std::vector<std::shared_ptr<AudioSegment> > CreateAudioSegmentSequence(
        double playbackStartTime, PlaybackDirection) override;

private:
    std::vector<std::shared_ptr<AudioSegment> >
    CreateAudioSegmentSequenceForward(double playbackStartTime);

    std::vector<std::shared_ptr<AudioSegment> >
    CreateAudioSegmentSequenceBackward(double playbackStartTime);

private:
    const ClipConstHolders mClips;
    const int mSampleRate;
    const int mNumChannels;
};
