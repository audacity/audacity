/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <functional>
#include <memory>

#include "au3-clip-processing/sequence-graph/ISequenceProcessingNode.h"

struct PlayableSequence;

namespace au::au4audio {
//! Its input with its track's volume and pan; where a mono track becomes stereo. In place, in stereo.
class GainPanNode final : public ISequenceProcessingNode
{
public:
    //! @param channelVolume of the volume and pan, by output channel, as PlayableSequence::GetChannelVolume
    GainPanNode(std::shared_ptr<ISequenceProcessingNode> input, size_t numInputChannels, std::function<float(int)> channelVolume);
    GainPanNode(std::shared_ptr<ISequenceProcessingNode> input, std::shared_ptr<const PlayableSequence> track);

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    const std::shared_ptr<ISequenceProcessingNode> m_input;
    const size_t m_numInputChannels;
    const std::function<float(int)> m_channelVolume;
};
}
