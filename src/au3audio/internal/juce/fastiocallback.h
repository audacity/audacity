/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "juceiocallback.h"

namespace au::au4juce {
//! Plays fast: the tracks' sequences are read ahead on workers, but the chains render in the device callback
/*!
 * Changes are heard within a block, and the input can be monitored through the effects. What isn't rendered in time is
 * skipped: a worker that can't keep up resumes ahead of the device.
 */
class FastIoCallback final : public JuceIoCallback
{
public:
    using JuceIoCallback::JuceIoCallback;
    ~FastIoCallback() override;

private:
    const char* name() const override { return "JUCE, fast"; }
    std::optional<TrackReadAhead> trackReadAhead() const override;
#ifdef AU_USE_TRACKTION
    //! In parallel, with tracktion_graph
    std::shared_ptr<ISequenceProcessingNode> makeMix(std::vector<std::shared_ptr<ISequenceProcessingNode> > tracks,
                                                     size_t maxFrames) override;
#endif
    void onBuilt(std::shared_ptr<ISequenceProcessingNode> top) override { m_top = std::move(top); }
    void startReadingAhead() override;
    void invalidateReadAhead() override;
    int64_t readAheadEnd() const override;
    size_t render(float* const* stereo, int64_t index, size_t frames) override;
    void releaseRendering() override { m_top.reset(); }

    std::shared_ptr<ISequenceProcessingNode> m_top;
};
}
