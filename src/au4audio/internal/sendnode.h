/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <functional>
#include <memory>

#include "au3-clip-processing/sequence-graph/ISequenceProcessingNode.h"

namespace au::au4audio {
//! What a track or a bus sends to a bus: its input at the send's level; a mono input in both channels. In place, in stereo.
class SendNode final : public ISequenceProcessingNode
{
public:
    //! @param numInputChannels 1 where it is taken from a mono track before its volume and pan
    //! @param level read as it renders
    SendNode(std::shared_ptr<ISequenceProcessingNode> input, size_t numInputChannels, std::function<float()> level);

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    const std::shared_ptr<ISequenceProcessingNode> m_input;
    const size_t m_numInputChannels;
    const std::function<float()> m_level;
};
}
