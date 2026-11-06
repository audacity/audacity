/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "au3-clip-processing/sequence-graph/LockFreeQueueNode.h"

#include "juceiocallback.h"

namespace au::au4juce {
//! Plays safe: everything renders ahead of the device on a worker, behind one queue, as AudioIO
/*!
 * Peaks of processing are absorbed by the queue; what isn't rendered in time plays late, after silence. Changes are heard
 * once what was rendered before them has played. The device callback only reads the queue, applies the playback volume,
 * and reports what plays.
 */
class SafeIoCallback final : public JuceIoCallback
{
public:
    using JuceIoCallback::JuceIoCallback;
    ~SafeIoCallback() override;

private:
    const char* name() const override { return "JUCE, safe"; }
    std::optional<TrackReadAhead> trackReadAhead() const override { return std::nullopt; }
    void onBuilt(std::shared_ptr<ISequenceProcessingNode> top) override;
    void startReadingAhead() override;
    void invalidateReadAhead() override;
    int64_t readAheadEnd() const override;
    size_t render(float* const* stereo, int64_t index, size_t frames) override;
    void releaseRendering() override;
    std::string dropoutDetails() const override;

    std::unique_ptr<LockFreeQueueNode> m_queue;
    //! Device thread: silence while waiting for the worker
    int64_t m_unplayedFrames = 0;
};
}
