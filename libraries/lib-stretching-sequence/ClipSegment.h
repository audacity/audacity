/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipSegment.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioSegment.h"
#include "AudioSegmentSampleView.h"
#include "PlaybackDirection.h"
#include "TimeAndPitchInterface.h"

#include <memory>

class ClipInterface;
using ChannelSampleViews = std::vector<AudioSegmentSampleView>;

class STRETCHING_SEQUENCE_API ClipSegment final :
    public AudioSegment,
    public TimeAndPitchSource
{
public:
   ClipSegment(
      const ClipInterface&, double durationToDiscard, PlaybackDirection);

   // AudioSegment
   size_t GetFloats(std::vector<float*>& buffers, size_t numSamples) override;
   bool Empty() const override;

private:
   // TimeAndPitchSource
   void Pull(float* const*, size_t samplesPerChannel) override;

   const ClipInterface& mClip;
   sampleCount mLastReadSample = 0;
   const sampleCount mTotalNumSamplesToProduce;
   sampleCount mTotalNumSamplesProduced = 0;
   const PlaybackDirection mPlaybackDirection;
   ChannelSampleViews mChannelSampleViews;
   // Careful that this guy is constructed last, as its ctor refers to *this.
   // todo(mhodgkinson) make this safe.
   std::unique_ptr<TimeAndPitchInterface> mStretcher;
};
