/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipSegment.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "ClipSegment.h"
#include "ClipInterface.h"
#include "SampleFormat.h"
#include "StaffPadTimeAndPitch.h"

#include <cassert>

namespace
{
TimeAndPitchInterface::Parameters
GetStretchingParameters(const ClipInterface& clip)
{
   TimeAndPitchInterface::Parameters params;
   params.timeRatio = clip.GetStretchRatio();
   return params;
}

sampleCount
GetTotalNumSamplesToProduce(const ClipInterface& clip, double durationToDiscard)
{
   return sampleCount { clip.GetVisibleSampleCount().as_double() *
                           clip.GetStretchRatio() -
                        durationToDiscard * clip.GetRate() + .5 };
}
} // namespace

ClipSegment::ClipSegment(
   const ClipInterface& clip, double durationToDiscard,
   PlaybackDirection direction)
    : mTotalNumSamplesToProduce { GetTotalNumSamplesToProduce(
         clip, durationToDiscard) }
    , mSource { clip, durationToDiscard, direction }
    , mStretcher { std::make_unique<StaffPadTimeAndPitch>(
         clip.GetWidth(), mSource, GetStretchingParameters(clip)) }
{
}

size_t ClipSegment::GetFloats(std::vector<float*>& buffers, size_t numSamples)
{
   assert(buffers.size() == mSource.GetWidth());
   const auto numSamplesToProduce = limitSampleBufferSize(
      numSamples, mTotalNumSamplesToProduce - mTotalNumSamplesProduced);
   mStretcher->GetSamples(buffers.data(), numSamplesToProduce);
   mTotalNumSamplesProduced += numSamplesToProduce;
   return numSamplesToProduce;
}

bool ClipSegment::Empty() const
{
   return mTotalNumSamplesProduced == mTotalNumSamplesToProduce;
}

size_t ClipSegment::GetWidth() const
{
   return mSource.GetWidth();
}
