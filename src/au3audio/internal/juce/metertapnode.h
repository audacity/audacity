/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <vector>

#include "au3-mixer/ISequenceProcessingNode.h"

#include "stereo.h"

namespace au::au4juce {
//! Its input, unchanged, of which it keeps what it passed for a meter to show
/*!
 * Kept by the thread that renders it, contiguous reads appended, until reset() or until it can't keep more: then it keeps
 * the latest read. Read once the block is rendered, by whoever sends it to the meter, who then resets it.
 */
class MeterTapNode final : public ISequenceProcessingNode
{
public:
    //! What was passed since the last reset, or since reading jumped
    struct Capture {
        //! The stream index of its first frame
        int64_t index = 0;
        size_t frames = 0;
        //! One channel for a mono track: of its panned stereo, the louder side, which has the peak and RMS its meter
        //! shows; two otherwise
        size_t numChannels = 0;
        const float* channels[au4juce::numChannels] {};
    };

    //! @param numMeterChannels 1 or 2; see Capture
    //! @param maxFrames the most it keeps: what a block reads of it
    MeterTapNode(std::shared_ptr<ISequenceProcessingNode> input, size_t numMeterChannels, size_t maxFrames);

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

    Capture capture() const;
    void reset() { m_frames = 0; }

private:
    const std::shared_ptr<ISequenceProcessingNode> m_input;
    const size_t m_numMeterChannels;
    std::vector<float> m_kept[numChannels];
    int64_t m_index = 0;
    size_t m_frames = 0;
};
}
