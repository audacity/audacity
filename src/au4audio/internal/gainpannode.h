/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "au3-clip-processing/sequence-graph/ISequenceProcessingNode.h"

struct PlayableSequence;

namespace au::au4audio {
//! Its input with its track's volume and pan; where a mono track becomes stereo. In place, in stereo.
class GainPanNode final : public ISequenceProcessingNode
{
public:
    GainPanNode(std::shared_ptr<ISequenceProcessingNode> input, std::shared_ptr<const PlayableSequence> track);

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    const std::shared_ptr<ISequenceProcessingNode> m_input;
    const std::shared_ptr<const PlayableSequence> m_track;
};
}
