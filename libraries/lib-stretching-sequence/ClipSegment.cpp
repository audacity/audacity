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

sampleCount GetLastReadSample(
   const ClipInterface& clip, double durationToDiscard,
   PlaybackDirection direction)
{
   if (direction == PlaybackDirection::forward)
      return sampleCount {
         clip.GetRate() * durationToDiscard / clip.GetStretchRatio() + .5
      };
   else
      return clip.GetVisibleSampleCount() - sampleCount {
         clip.GetRate() * durationToDiscard / clip.GetStretchRatio() + .5
      };
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
    : mClip { clip }
    , mLastReadSample { GetLastReadSample(clip, durationToDiscard, direction) }
    , mTotalNumSamplesToProduce { GetTotalNumSamplesToProduce(
         clip, durationToDiscard) }
    , mPlaybackDirection { direction }
    , mStretcher { std::make_unique<StaffPadTimeAndPitch>(
         clip.GetWidth(), *this, GetStretchingParameters(clip)) }
{
}

size_t ClipSegment::GetFloats(std::vector<float*>& buffers, size_t numSamples)
{
   assert(buffers.size() == mClip.GetWidth());
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
   return mClip.GetWidth();
}

void ClipSegment::Pull(float* const* buffers, size_t samplesPerChannel)
{
   const auto forward = mPlaybackDirection == PlaybackDirection::forward;
   const auto remainingSamplesInClip =
      forward ? mClip.GetVisibleSampleCount() - mLastReadSample :
                mLastReadSample;
   const auto numSamplesToRead =
      limitSampleBufferSize(samplesPerChannel, remainingSamplesInClip);
   if (numSamplesToRead > 0u)
   {
      constexpr auto mayThrow = false;
      const auto start =
         forward ? mLastReadSample : mLastReadSample - numSamplesToRead;
      const auto nChannels = mClip.GetWidth();
      ChannelSampleViews newViews;
      for (auto i = 0u; i < nChannels; ++i)
      {
         auto channelView = mClip.GetSampleView(i, start, numSamplesToRead);
         channelView.Copy(buffers[i], samplesPerChannel);
         newViews.push_back(std::move(channelView));
         if (!forward)
            ReverseSamples(
               reinterpret_cast<samplePtr>(buffers[i]), floatSample, 0,
               numSamplesToRead);
      }
      mChannelSampleViews = std::move(newViews);
      mLastReadSample +=
         forward ?
            sampleCount { numSamplesToRead } :
            // Cast to `sampleCount` before negating ;
            // on x86, size_t is 32 bits, but sampleCount will store it as 64
            // bits. Result : -1 becomes 2^32-1 and adding it to mLastReadSample
            // will not overflow, which is actually wanted in that case to
            // become a reasonably small value again :)
            -sampleCount { numSamplesToRead };
   }
   if (numSamplesToRead < samplesPerChannel)
   {
      for (auto i = 0u; i < mClip.GetWidth(); ++i)
         std::fill(buffers[i] + numSamplesToRead,
            buffers[i] + samplesPerChannel, 0.f);
   }
}
