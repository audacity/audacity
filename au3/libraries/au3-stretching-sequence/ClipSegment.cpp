/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipSegment.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "ClipSegment.h"
#include "ClipInterface.h"
#include "au3-time-and-pitch/StaffPadTimeAndPitch.h"
#include <algorithm>
#include <cmath>
#include <functional>

namespace {
TimeAndPitchInterface::Parameters
GetStretchingParameters(const ClipInterface& clip, double tempoScale)
{
    TimeAndPitchInterface::Parameters params;
    params.timeRatio = clip.GetStretchRatio() / tempoScale;
    params.pitchRatio = std::pow(2., clip.GetCentShift() / 1200.);
    params.preserveFormants
        =clip.GetPitchAndSpeedPreset() == PitchAndSpeedPreset::OptimizeForVoice;
    return params;
}

//! Remaining clip play samples before applying tempo scale.
sampleCount
GetUnityRemainingSamples(const ClipInterface& clip, double durationToDiscard)
{
    const double remaining
        =clip.GetVisibleSampleCount().as_double() * clip.GetStretchRatio()
          - durationToDiscard * clip.GetRate();
    return sampleCount { std::max(0.0, remaining) + .5 };
}

sampleCount
GetTotalNumSamplesToProduce(sampleCount unityRemaining, double tempoScale)
{
    return sampleCount {
        std::max(0.0, unityRemaining.as_double() / tempoScale) + .5
    };
}
} // namespace

ClipSegment::ClipSegment(
    const ClipInterface& clip, double durationToDiscard, PlaybackDirection direction,
    PlaybackTempoScale::Ptr tempoScale)
    : mTempoScale{tempoScale ? std::move(tempoScale) : PlaybackTempoScale::Create()}
    , mTotalNumSamplesToProduce{GetTotalNumSamplesToProduce(
                                    GetUnityRemainingSamples(clip, durationToDiscard),
                                    std::max(0.01, PlaybackTempoScale::Get(mTempoScale)))}
    , mSource{clip, durationToDiscard, direction}
    , mPreserveFormants{clip.GetPitchAndSpeedPreset()
                        == PitchAndSpeedPreset::OptimizeForVoice}
    , mCentShift{clip.GetCentShift()}
    , mBaseStretchRatio{clip.GetStretchRatio()}
    , mLastTempoScale{std::max(0.01, PlaybackTempoScale::Get(mTempoScale))}
    , mStretcher{std::make_unique<StaffPadTimeAndPitch>(
                     clip.GetRate(), clip.NChannels(), mSource,
                     GetStretchingParameters(clip, mLastTempoScale))}
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

    // Live Play-at-Speed with Preserve pitch: update time ratio and remaining
    // output budget when this operation's tempo scale changes.
    const double tempoScale = std::max(0.01, PlaybackTempoScale::Get(mTempoScale));
    if (tempoScale != mLastTempoScale) {
        // Output samples already produced were at the old scale. Remaining
        // unity-space samples scale as (T_old - P) * S_old / S_new.
        const double produced = mTotalNumSamplesProduced.as_double();
        const double remainingAtOld
            =std::max(0.0, mTotalNumSamplesToProduce.as_double() - produced);
        const double remainingAtNew
            =remainingAtOld * mLastTempoScale / tempoScale;
        mTotalNumSamplesToProduce
            =sampleCount { produced + remainingAtNew + .5 };
        mLastTempoScale = tempoScale;
        mStretcher->OnTimeRatioChange(mBaseStretchRatio / tempoScale);
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
