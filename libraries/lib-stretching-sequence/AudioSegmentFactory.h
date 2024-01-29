/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioSegmentFactory.h

  Matthieu Hodgkinson

**********************************************************************/

#pragma once

#include "AudioSegmentFactoryInterface.h"

class ClipInterface;
using ClipHolders = std::vector<std::shared_ptr<ClipInterface>>;

class STRETCHING_SEQUENCE_API AudioSegmentFactory final :
    public AudioSegmentFactoryInterface
{
public:
   AudioSegmentFactory(int sampleRate, int numChannels, ClipHolders clips);

   std::vector<std::shared_ptr<AudioSegment>> CreateAudioSegmentSequence(
      double playbackStartTime, PlaybackDirection) const override;

private:
   std::vector<std::shared_ptr<AudioSegment>>
   CreateAudioSegmentSequenceForward(double playbackStartTime) const;

   std::vector<std::shared_ptr<AudioSegment>>
   CreateAudioSegmentSequenceBackward(double playbackStartTime) const;

private:
   const ClipHolders mClips;
   const int mSampleRate;
   const int mNumChannels;
};
