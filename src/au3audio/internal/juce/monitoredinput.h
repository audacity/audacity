/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <vector>

#include "au3-mixer/ISequenceProcessingNode.h"

#include "stereo.h"

namespace au::au4juce {
//! The device's input, as monitored through the effects: that of the block being rendered, with the input volume
/*!
 * Written by the device callback before it renders a block, read by the chains while they render it: by the same
 * thread, or by the pool's while the callback waits for them.
 */
class MonitoredInput
{
public:
    //! @param maxFrames the most a block has
    MonitoredInput(int numInputChannels, size_t maxFrames, const std::atomic<float>& inputVolume);

    //! How many channels the device must give
    int numInputChannels() const { return m_numInputChannels; }

    //! Device thread: `frames` frames of `input`, from `offset` on, for the block that renders from stream index `index`;
    //! silence if `input` is null. A mono input is in both channels, as play-through does.
    void read(const float* const* input, int offset, int frames, int64_t index);

    //! Into the first `numChannels` of `buffers`, what was read from stream index `index` for `len` frames; false, and
    //! nothing written, unless that is the block's
    bool get(float* const* buffers, size_t numChannels, int64_t index, size_t len) const;

private:
    const int m_numInputChannels;
    const std::atomic<float>& m_inputVolume;
    std::vector<float> m_input[numChannels];
    int64_t m_index = 0;
    size_t m_frames = 0;
};

//! The monitored input, as a source in a chain: live where the chain is read as it renders the block, silent elsewhere,
//! e.g. where effects are primed
class MonitoredInputNode final : public ISequenceProcessingNode
{
public:
    //! @param numChannels how many of the buffers it fills: a mono track's effects get silence in the second
    //! @param lead how far ahead of the stream index the chain reads it: by the latency of the effects after it
    MonitoredInputNode(std::shared_ptr<const MonitoredInput> input, size_t numChannels, int64_t lead);

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    const std::shared_ptr<const MonitoredInput> m_input;
    const size_t m_numChannels;
    const int64_t m_lead;
};
}
