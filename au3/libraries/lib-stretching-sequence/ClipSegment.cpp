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
#include <cmath>
#include <functional>

namespace {
TimeAndPitchInterface::Parameters
GetStretchingParameters(const ClipInterface& clip)
{
    TimeAndPitchInterface::Parameters params;
    params.timeRatio = clip.GetStretchRatio();
    params.pitchRatio = std::pow(2., clip.GetCentShift() / 1200.);
    params.preserveFormants
        =clip.GetPitchAndSpeedPreset() == PitchAndSpeedPreset::OptimizeForVoice;
    return params;
}

sampleCount
GetTotalNumSamplesToProduce(const ClipInterface& clip, double durationToDiscard)
{
    return sampleCount { clip.GetVisibleSampleCount().as_double()
                         * clip.GetStretchRatio()
                         - durationToDiscard * clip.GetRate() + .5 };
}
} // namespace

ClipSegment::ClipSegment(
    const ClipInterface& clip, double durationToDiscard, PlaybackDirection direction)
    : mTotalNumSamplesToProduce{GetTotalNumSamplesToProduce(
                                    clip, durationToDiscard)}
    , mSource{clip, durationToDiscard, direction}
    , mPreserveFormants{clip.GetPitchAndSpeedPreset()
                        == PitchAndSpeedPreset::OptimizeForVoice}
    , mCentShift{clip.GetCentShift()}
    , mStretcher{std::make_unique<StaffPadTimeAndPitch>(
                     clip.GetRate(), clip.NChannels(), mSource,
                     GetStretchingParameters(clip))}
    , mOnSemitoneShiftChangeSubscription{clip.SubscribeToCentShiftChange(
                                             [this](int cents) {
        mCentShift = cents;
        mUpdateCentShift = true;
    })},
    mOnFormantPreservationChangeSubscription {
    clip.SubscribeToPitchAndSpeedPresetChange(
        [this](PitchAndSpeedPreset preset) {
        mPreserveFormants
            =preset == PitchAndSpeedPreset::OptimizeForVoice;
        mUpdateFormantPreservation = true;
    })
}
{
}

ClipSegment::~ClipSegment()
{
    mOnSemitoneShiftChangeSubscription.Reset();
    mOnFormantPreservationChangeSubscription.Reset();
}

size_t ClipSegment::GetFloats(float* const* buffers, size_t numSamples)
{
    // Check if formant preservation of pitch shift needs to be updated.
    // This approach is not immune to a race condition, but it is unlikely and
    // not critical, as it would only affect one playback pass, during which the
    // user could easily correct the mistake if needed. On the other hand, we
    // cannot trust that the observer subscriptions do not get called after
    // destruction of this object, so better not do anything too sophisticated
    // there.
    if (mUpdateFormantPreservation.exchange(false)) {
        mStretcher->OnFormantPreservationChange(mPreserveFormants);
    }
    if (mUpdateCentShift.exchange(false)) {
        mStretcher->OnCentShiftChange(mCentShift);
    }
    const auto numSamplesToProduce = limitSampleBufferSize(
        numSamples, mTotalNumSamplesToProduce - mTotalNumSamplesProduced);
    mStretcher->GetSamples(buffers, numSamplesToProduce);
    mTotalNumSamplesProduced += numSamplesToProduce;
    return numSamplesToProduce;
}

bool ClipSegment::Empty() const
{
    return mTotalNumSamplesProduced == mTotalNumSamplesToProduce;
}

size_t ClipSegment::NChannels() const
{
    return mSource.NChannels();
}
