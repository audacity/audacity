/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <vector>

#include "au3-clip-processing/sequence-graph/ISequenceProcessingNode.h"

#include "effectscratch.h"
#include "stereo.h"

class ChannelGroup;

namespace au::au4audio {
class IEffectsContext;

//! Its input through the realtime effects of a group: a track's, or the master's
/*!
 * What is read at an index is heard there: as AudioIO does, the input is read ahead by the effects' latency, and
 * wherever reading jumps, the effects are first fed for as long, and what they output then is dropped. The latency is the
 * effects' when the node is built.
 *
 * Read in stereo, at most `maxFrames` at once.
 */
class EffectsNode final : public ISequenceProcessingNode
{
public:
    //! @param numInputChannels how many of the buffers the input fills; the others are silent
    EffectsNode(std::shared_ptr<ISequenceProcessingNode> input, size_t numInputChannels, const ChannelGroup* group,
                IEffectsContext& context, size_t maxFrames);

    int latency() const { return m_latency; }

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    //! Renders what is heard from `index` on, at most maxFrames
    void render(float* const* output, int64_t index, size_t frames);
    //! Reads the input from `index` on into `output`, and processes it
    void process(float* const* output, int64_t index, size_t frames);

    const std::shared_ptr<ISequenceProcessingNode> m_input;
    const size_t m_numInputChannels;
    const ChannelGroup* const m_group;
    IEffectsContext& m_context;
    const int m_latency;
    const size_t m_maxFrames;
    //! Where reading continues without jumping
    std::optional<int64_t> m_next;
    EffectScratch m_scratch;
    std::vector<float> m_discarded[numChannels];
    float* m_discardedPointers[numChannels] {};
};
}
