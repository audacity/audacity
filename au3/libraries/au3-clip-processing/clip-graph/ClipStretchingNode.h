/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "IClipProcessingNode.h"
#include "au3-utility/Observer.h"
#include <atomic>
#include <memory>
#include <optional>

class ClipInterface;

/*!
 * The source node: a clip, time-stretched and pitch-shifted.
 *
 * Stretching is stateful: it is a stream, which reading where the previous
 * read ended continues. Reading elsewhere restarts it there.
 *
 * It is important that objects of this class are instantiated and destroyed on
 * the same thread, due to the owned Observer::Subscription.
 */
class ClipStretchingNode final : public IClipProcessingNode
{
public:
    explicit ClipStretchingNode(const ClipInterface&);
    ~ClipStretchingNode() override;

    // IClipProcessingNode
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;
    sampleCount NumSamples() const override;
    size_t NChannels() const override;

private:
    struct TimeStretchingStream;

    const ClipInterface& mClip;
    std::unique_ptr<TimeStretchingStream> mStream;
    //! Where the stream continues
    std::optional<sampleCount> mExpectedStart;

    bool mPreserveFormants;
    int mCentShift;
    std::atomic<bool> mUpdateFormantPreservation = false;
    std::atomic<bool> mUpdateCentShift = false;
    Observer::Subscription mOnSemitoneShiftChangeSubscription;
    Observer::Subscription mOnFormantPreservationChangeSubscription;
};
