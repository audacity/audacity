/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipTimeAndPitchSource.cpp

  Matthieu Hodgkinson

**********************************************************************/

#include "ClipTimeAndPitchSource.h"
#include "ClipInterface.h"

#include <cassert>

namespace {
sampleCount GetLastReadSample(
    const ClipInterface& clip, double durationToDiscard,
    PlaybackDirection direction)
{
    if (direction == PlaybackDirection::forward) {
        return sampleCount {
            clip.GetRate() * durationToDiscard / clip.GetStretchRatio() + .5
        };
    } else {
        return clip.GetVisibleSampleCount() - sampleCount {
            clip.GetRate() * durationToDiscard / clip.GetStretchRatio() + .5
        };
    }
}
} // namespace

ClipTimeAndPitchSource::ClipTimeAndPitchSource(
    const ClipInterface& clip, double durationToDiscard,
    PlaybackDirection direction)
    : mClip{clip}
    , mLastReadSample{GetLastReadSample(clip, durationToDiscard, direction)}
    , mPlaybackDirection{direction}
{
}

void ClipTimeAndPitchSource::Pull(
    float* const* buffers, size_t samplesPerChannel)
{
    const auto forward = mPlaybackDirection == PlaybackDirection::forward;
    const auto remainingSamplesInClip
        =forward ? mClip.GetVisibleSampleCount() - mLastReadSample
          : mLastReadSample;
    const auto numSamplesToRead
        =limitSampleBufferSize(samplesPerChannel, remainingSamplesInClip);
    if (numSamplesToRead > 0u) {
        constexpr auto mayThrow = false;
        const auto start
            =forward ? mLastReadSample : mLastReadSample - numSamplesToRead;
        const auto nChannels = mClip.NChannels();
        ChannelSampleViews newViews;
        for (auto i = 0u; i < nChannels; ++i) {
            auto channelView = mClip.GetSampleView(i, start, numSamplesToRead);
            // Query `samplesPerChannel` samples ; `AudioSegmentSampleView::Copy`
            // will zero from `numSamplesToRead` to `samplesPerChannel` if needed.
            channelView.Copy(buffers[i], samplesPerChannel);
            newViews.push_back(std::move(channelView));
            if (!forward) {
                ReverseSamples(
                    reinterpret_cast<samplePtr>(buffers[i]), floatSample, 0,
                    numSamplesToRead);
            }
        }
        mChannelSampleViews = std::move(newViews);
        mLastReadSample
            +=forward
               ? sampleCount { numSamplesToRead }
        :   // Cast to `sampleCount` before negating ;
            // on x86, size_t is 32 bits, but sampleCount will store it as 64
            // bits. Result : -1 becomes 2^32-1 and adding it to mLastReadSample
            // will not overflow, which is actually wanted in that case to
            // become a reasonably small value again :)
        -sampleCount { numSamplesToRead };
    } else {
        for (auto i = 0u; i < mClip.NChannels(); ++i) {
            std::fill(buffers[i], buffers[i] + samplesPerChannel, 0.f);
        }
    }
}

size_t ClipTimeAndPitchSource::NChannels() const
{
    return mClip.NChannels();
}
