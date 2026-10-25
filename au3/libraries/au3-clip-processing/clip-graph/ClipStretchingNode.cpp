/*
 * Audacity: A Digital Audio Editor
 */
#include "ClipStretchingNode.h"
#include "ClipInterface.h"
#include "ClipTimeAndPitchSource.h"
#include "au3-time-and-pitch/StaffPadTimeAndPitch.h"
#include <cmath>

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
} // namespace

/**
 * @brief Stateful objects needed for time stretching.
 * @detail We use the term "stream" because, as opposed to the random-access-looking
 * `IClipProcessingNode::GetSamples(float* const* buffers, sampleCount start, size_t len)` interface,
 * the stretcher is inherently sequential.
 */
struct ClipStretchingNode::TimeStretchingStream {
    //! @param start relative to the clip's play start
    TimeStretchingStream(const ClipInterface& clip, sampleCount start)
        : source{clip, start.as_double() / clip.GetRate(), PlaybackDirection::forward}
        , stretcher{clip.GetRate(), clip.NChannels(), source, GetStretchingParameters(clip)}
    {
    }

    ClipTimeAndPitchSource source;
    // Refers to `source`, constructed before it
    StaffPadTimeAndPitch stretcher;
};

ClipStretchingNode::ClipStretchingNode(const ClipInterface& clip)
    : mClip{clip}
    , mPreserveFormants{clip.GetPitchAndSpeedPreset() == PitchAndSpeedPreset::OptimizeForVoice}
    , mCentShift{clip.GetCentShift()}
{
    mOnSemitoneShiftChangeSubscription = clip.SubscribeToCentShiftChange(
        [this](int cents) {
        mCentShift = cents;
        mUpdateCentShift = true;
    });

    mOnFormantPreservationChangeSubscription = clip.SubscribeToPitchAndSpeedPresetChange(
        [this](PitchAndSpeedPreset preset) {
        mPreserveFormants = preset == PitchAndSpeedPreset::OptimizeForVoice;
        mUpdateFormantPreservation = true;
    });
}

ClipStretchingNode::~ClipStretchingNode() = default;

void ClipStretchingNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    if (!mStream || mExpectedStart != start) {
        mStream = std::make_unique<TimeStretchingStream>(mClip, start);
    }
    mExpectedStart = start + static_cast<sampleCount::type>(len);

    // Check if formant preservation of pitch shift needs to be updated.
    // This approach is not immune to a race condition, but it is unlikely and
    // not critical, as it would only affect one playback pass, during which the
    // user could easily correct the mistake if needed. On the other hand, we
    // cannot trust that the observer subscriptions do not get called after
    // destruction of this object, so better not do anything too sophisticated
    // there.
    if (mUpdateFormantPreservation.exchange(false)) {
        mStream->stretcher.OnFormantPreservationChange(mPreserveFormants);
    }
    if (mUpdateCentShift.exchange(false)) {
        mStream->stretcher.OnCentShiftChange(mCentShift);
    }
    mStream->stretcher.GetSamples(buffers, len);
}

sampleCount ClipStretchingNode::NumSamples() const
{
    return sampleCount { mClip.GetVisibleSampleCount().as_double() * mClip.GetStretchRatio() + .5 };
}

size_t ClipStretchingNode::NChannels() const
{
    return mClip.NChannels();
}
