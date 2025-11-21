/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipSegment.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioSegment.h"
#include "ClipTimeAndPitchSource.h"
#include "Observer.h"
#include "PlaybackDirection.h"
#include <atomic>
#include <memory>

class ClipInterface;
class TimeAndPitchInterface;

using PitchRatioChangeCbSubscriber
    =std::function<void (std::function<void (double)>)>;

/*!
 * It is important that objects of this class are instantiated and destroyed on
 * the same thread, due to the owned Observer::Subscription.
 */
class STRETCHING_SEQUENCE_API ClipSegment final : public AudioSegment
{
public:
    ClipSegment(const ClipInterface&, double durationToDiscard, PlaybackDirection);
    ~ClipSegment() override;

    // AudioSegment
    size_t GetFloats(float* const* buffers, size_t numSamples) override;
    bool Empty() const override;
    size_t NChannels() const override;

private:
    const sampleCount mTotalNumSamplesToProduce;
    sampleCount mTotalNumSamplesProduced = 0;
    ClipTimeAndPitchSource mSource;
    bool mPreserveFormants;
    int mCentShift;
    std::atomic<bool> mUpdateFormantPreservation = false;
    std::atomic<bool> mUpdateCentShift = false;
    // Careful that this guy is constructed after `mSource`, which it refers to
    // in its ctor.
    // todo(mhodgkinson) make this safe.
    std::unique_ptr<TimeAndPitchInterface> mStretcher;
    Observer::Subscription mOnSemitoneShiftChangeSubscription;
    Observer::Subscription mOnFormantPreservationChangeSubscription;
};
